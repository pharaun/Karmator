{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server.Slack
    ( runServer
    , SlackConfig
    , getServerConfig
    ) where

import Safe
import System.IO
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad.Reader
import Data.Typeable
import Prelude hiding (log, head, tail)
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as DL

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

--
-- TODO: For config bits
--
import Data.ConfigFile
--
-- Config bits above
--

-- TODO: kill this here
import qualified Data.ByteString.Char8 as C8

import Pipes
import qualified Pipes.Network.TCP as PNT

-- Slack Parser
import qualified Web.Slack as WS

-- Irc Message stuff for transformation
import qualified Network.IRC as IRC

-- For the userid and channel id bits
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- Karmator Stuff
import Karmator.Types
import Karmator.Server hiding (runServer)
import qualified Karmator.Server as KS



--
-- Server Specific configs
--
data SlackConfig = SlackConfig
    { network :: String
    , apiToken :: String
    }
    deriving (Show)


--
-- Slack Map between username/channel and their ids
--
data SlackMap = SlackMap
    { uidToName :: Map T.Text T.Text
    , nameToUid :: Map T.Text T.Text
--    , cidToChan :: Map T.Text T.Text
--    , chanToCid :: Map T.Text T.Text
    }
    deriving (Show)


newSlackMap :: SlackMap
newSlackMap = SlackMap Map.empty Map.empty


initFromSession :: WS.SlackSession -> TVar SlackMap -> IO ()
initFromSession s m = do
    let uidToName = map getName (WS._slackUsers s)

    atomically $ writeTVar m $ SlackMap (Map.fromList uidToName) (Map.fromList $ map (\(a, b) -> (b, a)) uidToName)
  where
    getName :: WS.User -> (T.Text, T.Text)
    getName u = (WS._getId $ WS._userId u, WS._userName u)


getUserX :: (SlackMap -> Map T.Text T.Text) -> WS.SlackHandle -> TVar SlackMap -> T.Text -> IO T.Text
getUserX getMap h m val = do
    sm <- readTVarIO m
    let v = Map.lookup val (getMap sm)

    case v of
        Just x  -> return x
        -- TODO: Do query
        Nothing -> return "test"


getUserName = getUserX uidToName
getUserId = getUserX nameToUid


--
-- Establish and run a server connection
--
runServer = KS.runServer establishConnection emitConnectionLoss newSlackMap


--
-- Establish the connection
--
-- TODO: unknown what kind of connection options are there
--
-- slack-api - raises an exception if there was a network error (caught by syncIO)
--
establishConnection :: ServerState SlackConfig IRC.Message SlackMap -> IO ServerEvent
establishConnection ss@ServerState{config=ServerConfig{serverSpecific=sc}} = do
    let conf = WS.SlackConfig{ WS._slackApiToken = apiToken sc}

    WS.withSlackHandle conf (handleSlack ss)


-- TODO: Once confirm/start launch/reconn make sure to emit(
    -- Emit connection established here
    --emitConnectionEstablished ss
-- )
handleSlack :: ServerState SlackConfig IRC.Message SlackMap -> WS.SlackHandle -> IO ServerEvent
handleSlack ss@ServerState{config=ssc, logStream=l, botState=bs} h = do
    emitConnectionEstablished ss

    -- Start bot streaming
    loser <- race
        (runEffect (slackProducer h >-> logShow "" l >-> slackToIrc (network $ serverSpecific ssc) bs h >-> logFormat "\t" eIrcFormat l >-> messagePump ss))
        (runEffect (messageVacuum ss >-> onlyMessages >-> logFormat "\t\t" ircFormat l >-> ircToSlack h bs >-> logShow "\t\t\t" l >-> slackConsumer h))

    -- Identify who terminated first (the send or the recv)
    return $ case loser of
        Left _  -> RecvLost
        Right _ -> SendLost


--
-- Pump messages from slack into pipe streams
--
slackProducer :: WS.SlackHandle -> Producer WS.Event IO ()
slackProducer h = forever $ lift (WS.getNextEvent h) >>= yield


--
-- Pump Messages into Slack
--
slackConsumer :: WS.SlackHandle -> Consumer (T.Text, T.Text) IO ()
slackConsumer h = forever $ do
    (cid, msg) <- await
    lift $ WS.sendMessage h (WS.Id cid) msg


--
-- Transform Slack events into Irc messages to pump it
--
slackToIrc :: MonadIO m => String -> TVar SlackMap -> WS.SlackHandle -> Pipe WS.Event (BotEvent IRC.Message) m r
slackToIrc snet sm h = forever $ do
    e <- await

    -- TODO: Handle user id.
    -- 1) on hello, call initFromSession, use this to update the SlackMap on a Hello event
    -- 2) In the Message, do a getUserName to query for the irc one
    -- 3) In the other pipe func (do getUserId) to query for slack one


    -- TODO: handle BotIds
    -- TODO: see a way to remap the <@userid> -> an actual user name
    -- TODO: find a good way to handle remapping the user-id+channel id to an actual name (May need some new columns)
    -- TODO: Going to need to find another new message type to pump the id stuff through (hacky might be enough) then
    --      a new hook to feed those id updates/info into the database that can be used for lookups
    case e of
        WS.Hello -> liftIO $ initFromSession (WS.getSession h) sm
        WS.Message (WS.Id {WS._getId = cid}) (WS.UserComment (WS.Id {WS._getId = uid})) msg _ _ _ -> (do
            -- All fields are Text, get a nice converter to pack into utf8 bytestring

            user <- liftIO $ getUserName h sm uid

            let prefix = IRC.NickName (TE.encodeUtf8 user) (Just (TE.encodeUtf8 uid)) (Just (C8.pack "SlackServer"))
            let message = IRC.Message (Just prefix) (C8.pack "PRIVMSG") [TE.encodeUtf8 cid, TE.encodeUtf8 msg]

            -- TODO: support network name (for slack differnation)
            yield (EMessage snet message)
            )
        _ -> return ()


--
-- Transform Irc messages into Slack messages to pump to slack
--
-- TODO: Make actual Mechanic to tell the sender what to actually send othe rthan just message
--
ircToSlack :: MonadIO m => WS.SlackHandle -> TVar SlackMap -> Pipe IRC.Message (T.Text, T.Text) m r
ircToSlack h sm = forever $ do
    m <- await

    -- TODO: more fancy support
    case m of
        -- TODO: put the user name back into the prefix for use here
        IRC.Message _ _ msg -> (do
                let cid = headDef "" msg
                -- Drop the username portion of the message and replace with <@uid>
                let user = (BS.takeWhile (colon /=)) $ headDef "" $ tailSafe msg
                let msg' = (BS.dropWhile (space /=)) $ headDef "" $ tailSafe msg

                userId <- liftIO $ getUserId h sm "aberens"

                yield (TE.decodeUtf8 cid, TE.decodeUtf8 $ BS.concat [ "<@", TE.encodeUtf8 userId, ">:", msg' ])
            )
        _ -> return ()
  where
    space = BS.head " "
    colon = BS.head ":"


--
-- Filter any non Message
--
-- TODO: Identify if its a disconnect message and specifically disconnect
--
onlyMessages :: Monad m => Pipe (BotCommand IRC.Message) IRC.Message m ()
onlyMessages = forever $ do
    c <- await
    case c of
        CMessage m -> yield m
        _          -> return ()


--
-- Log Irc message
--
eIrcFormat :: BotEvent IRC.Message -> String
eIrcFormat x =
    case x of
        EMessage _ x' -> C8.unpack (IRC.encode x')
        _ -> ""

ircFormat :: IRC.Message -> String
ircFormat = C8.unpack . IRC.encode


--
-- Config
--
getServerConfig
    :: ConfigParser
    -> String
    -> ExceptT CPError IO (ServerConfig SlackConfig)
getServerConfig c s = do
    apitoken  <- get c s "api_token"
    logfile   <- get c s "logfile"
    logslack  <- get c s "logslack"
    reconn    <- get c s "reconn"
    reWait    <- get c s "reconn_wait" -- In seconds

    let network = DL.dropWhile ('.' ==) s
    let config = SlackConfig network apitoken
    return $ ServerConfig config reconn (reWait * 1000000) logfile logslack
