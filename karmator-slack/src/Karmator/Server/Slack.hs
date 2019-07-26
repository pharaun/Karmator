{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server.Slack
    ( runServer
    , SlackConfig(..)
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

import Pipes
import qualified Pipes.Network.TCP as PNT

-- Slack Parser
import qualified Web.Slack.Types.Time as WSTT
import qualified Web.Slack as WS
import qualified Web.Slack.WebAPI as WSA
import qualified Network.Wreq as W
import Control.Lens ((.~))
import qualified Data.Aeson as A

-- Slack message management stuff
import Slack.Message hiding (Message)
import qualified Slack.Message as Slack

-- Karmator Stuff
import Karmator.Types
import Karmator.Server hiding (runServer)
import qualified Karmator.Server as KS



--
-- Server Specific configs
--
data SlackConfig = SlackConfig
    { apiToken :: String
    }
    deriving (Show)


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
establishConnection :: ServerState SlackConfig Slack.Message SlackMap -> IO ServerEvent
establishConnection ss@ServerState{config=ServerConfig{serverSpecific=sc}} = do
    let conf = WS.SlackConfig{ WS._slackApiToken = apiToken sc}

    WS.withSlackHandle conf (handleSlack ss)


-- TODO: Once confirm/start launch/reconn make sure to emit(
    -- Emit connection established here
    --emitConnectionEstablished ss
-- )
handleSlack :: ServerState SlackConfig Slack.Message SlackMap -> WS.SlackHandle -> IO ServerEvent
handleSlack ss@ServerState{config=ssc, logStream=l, botState=bs} h = do
    emitConnectionEstablished ss

    -- Start bot streaming
    loser <- race
--        (runEffect (slackProducer h >-> logShow "" l >-> slackToIrc bs h l >-> logFormat "\t" eIrcFormat l >-> messagePump ss))
--        (runEffect (messageVacuum ss >-> onlyMessages >-> logFormat "\t\t" ircFormat l >-> ircToSlack h bs >-> logShow "\t\t\t" l >-> slackConsumer h))
        (runEffect (slackProducer h >-> slackToIrc bs h l >-> messagePump ss))
        (runEffect (messageVacuum ss >-> onlyMessages >-> ircToSlack h bs >-> slackConsumer h))

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
slackConsumer :: WS.SlackHandle -> Consumer (T.Text, T.Text, Maybe WSTT.SlackTimeStamp) IO ()
slackConsumer h = forever $ do
    (cid, msg, tts) <- await
    case tts of
        Just tts' -> lift $ WS.sendThreadedMessage h (WS.Id cid) tts' msg
        Nothing   -> lift $ WS.sendMessage h (WS.Id cid) msg


--
-- Transform Slack events into Irc messages to pump it
--
slackToIrc :: MonadIO m => TVar SlackMap -> WS.SlackHandle -> Handle -> Pipe WS.Event (BotEvent Slack.Message) m r
slackToIrc sm h l = forever $ do
    e <- await

    -- TODO: handle BotIds
    -- TODO: find a good way to handle remapping the user-id+channel id to an actual name (May need some new columns)
    -- TODO: Going to need to find another new message type to pump the id stuff through (hacky might be enough) then
    --      a new hook to feed those id updates/info into the database that can be used for lookups
    case e of
        WS.Hello -> liftIO $ initFromSession (WS.getSession h) sm
        WS.ThreadedMessage (WS.Id {WS._getId = cid}) (WS.UserComment (WS.Id {WS._getId = uid})) msg ts _ _ sts
                 -> emitMsg sm h l cid uid msg ts (Just sts)
        WS.Message (WS.Id {WS._getId = cid}) (WS.UserComment (WS.Id {WS._getId = uid})) msg ts _ _
                 -> emitMsg sm h l cid uid msg ts Nothing
        _ -> return ()
  where
    emitMsg sm h l cid uid msg ts sts = do
        -- All fields are Text, get a nice converter to pack into utf8 bytestring
        user <- liftIO $ getUserName l h sm uid
        -- TODO: channel <- liftIO $ getChannelName l h sm cid

        -- Parse message for <@id> and replace with userName
        msg' <- remapMessage sm h l msg

        -- TODO: find+identify if its threaded and populate this field
        let message = Slack.Message "PRIVMSG" Nothing cid user uid msg' ts sts
        yield (EMessage message)

--
-- Transform Irc messages into Slack messages to pump to slack
--
-- TODO: Make actual Mechanic to tell the sender what to actually send othe rthan just message
--
ircToSlack :: MonadIO m => WS.SlackHandle -> TVar SlackMap -> Pipe Slack.Message (T.Text, T.Text, Maybe WSTT.SlackTimeStamp) m r
ircToSlack h sm = forever $ do
    m <- await

    -- TODO: more fancy support
    case m of
        -- TODO: put the user name back into the prefix for use here
        msg@Slack.Message{} -> (do
            let cid = Slack.msg_cid msg
            let msg' = remapAtHere $ Slack.msg_text msg
            let sts = Slack.msg_thread_ts msg

            -- TODO: make this handle threads
            yield (cid, T.concat [ "<@", (Slack.msg_uid msg), ">: ", msg' ], sts)
            )
        _ -> return ()

--
-- Filter any non Message
--
-- TODO: Identify if its a disconnect message and specifically disconnect
--
onlyMessages :: Monad m => Pipe (BotCommand Slack.Message) Slack.Message m ()
onlyMessages = forever $ do
    c <- await
    case c of
        CMessage m -> yield m
        _          -> return ()


--
-- Log Irc message
--
eIrcFormat :: BotEvent Slack.Message -> String
eIrcFormat x =
    case x of
        EMessage x' -> show x'
        _ -> ""

ircFormat :: Slack.Message -> String
ircFormat = show
