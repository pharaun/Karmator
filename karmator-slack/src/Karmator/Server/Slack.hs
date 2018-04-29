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

-- Karmator Stuff
import Karmator.Types


--
-- Server Specific configs
--
data SlackConfig = SlackConfig
    { network :: String
    , apiToken :: String
    }
    deriving (Show)

--
-- Establish and run a server connection
--
runServer :: ServerConfig SlackConfig -> TQueue (BotEvent IRC.Message, TQueue (BotCommand IRC.Message)) -> IO ()
runServer sc queue = PNT.withSocketsDo $
    -- Establish the logfile
    withFile (logfile sc) AppendMode $ \l -> do
        -- Set log to be unbuffered for testing
        hSetBuffering l NoBuffering

        -- Server queue
        sq <- newTQueueIO
        suc <- newTVarIO False
        let ss = ServerState sc l queue sq suc

        -- Establish connection
        forever $ do
            -- Upon exit of the establishConnection we ensure we always emit ConnectionLost
            errors <- runExceptT $ syncIO $ C.finally (establishConnection ss) (emitConnectionLoss ss)

            -- TODO: good place for tallying the number of failure and
            -- shutting the server connection off if it exceeds some
            -- threshold.
            case errors of
                -- TODO: Need to identify the type of error and allow some to propagate upward - IE don't just catch all.
                Left e         -> logException l e
                Right RecvLost -> liftIO $ BS.hPutStr l "Recv was lost (closed server side)\n"
                Right SendLost -> liftIO $ BS.hPutStr l "Send was lost (closed server side)\n"

            -- Wait for a bit before retrying
            threadDelay $ reconnectWait sc

--
-- Establish the connection
--
-- TODO: unknown what kind of connection options are there
--
-- slack-api - raises an exception if there was a network error (caught by syncIO)
--
establishConnection :: ServerState SlackConfig IRC.Message -> IO ServerEvent
establishConnection ss@ServerState{config=ServerConfig{serverSpecific=sc}} = do
    let conf = WS.SlackConfig{ WS._slackApiToken = apiToken sc}

    WS.withSlackHandle conf (pipeBot ss)


-- TODO: build runBot from Web.Slack (feed it the needed server state in its event handler)
-- TODO: Wire up the event handler to the loop
-- TODO: Once confirm/start launch/reconn make sure to emit(
    -- Emit connection established here
    --emitConnectionEstablished ss
-- )
-- TODO: Figure out some sort of logging of data both way
pipeBot :: ServerState SlackConfig IRC.Message -> WS.SlackHandle -> IO ServerEvent
pipeBot ss@ServerState{config=ssc, logStream=l} h = do
    emitConnectionEstablished ss

    -- Start bot streaming
    loser <- race
        (runEffect (slackProducer h >-> logSlack l >-> slackToIrc (network $ serverSpecific ssc) >-> logEvent l >-> messagePump ss))
        -- TODO: create a slackConsumer
        (runEffect (messageVacuum ss >-> onlyMessages >-> logIrc l >-> ircToSlack >-> logReply l >-> slackConsumer h))

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
slackToIrc :: MonadIO m => String -> Pipe WS.Event (BotEvent IRC.Message) m r
slackToIrc snet = forever $ do
    e <- await

    -- TODO: handle BotIds
    -- TODO: see a way to remap the <@userid> -> an actual user name
    -- TODO: find a good way to handle remapping the user-id+channel id to an actual name (May need some new columns)
    -- TODO: Going to need to find another new message type to pump the id stuff through (hacky might be enough) then
    --      a new hook to feed those id updates/info into the database that can be used for lookups
    case e of
        WS.Message (WS.Id {WS._getId = cid}) (WS.UserComment (WS.Id {WS._getId = uid})) msg _ _ _ -> (do
            -- All fields are Text, get a nice converter to pack into utf8 bytestring
            let prefix = IRC.NickName (TE.encodeUtf8 uid) (Just (TE.encodeUtf8 uid)) (Just (C8.pack "SlackServer"))
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
ircToSlack :: MonadIO m => Pipe IRC.Message (T.Text, T.Text) m r
ircToSlack = forever $ do
    m <- await

    -- TODO: more fancy support
    case m of
        IRC.Message _ _ msg -> (do
                let cid = headDef "" msg
                -- Drop the username portion of the message and replace with <@uid>
                let uid = (BS.takeWhile (colon /=)) $ headDef "" $ tailSafe msg
                let msg' = (BS.dropWhile (space /=)) $ headDef "" $ tailSafe msg

                yield (TE.decodeUtf8 cid, TE.decodeUtf8 $ BS.concat [ "<@", uid, ">:", msg' ])
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
-- Log anything that passes through this stream to a logfile
--
logSlack :: MonadIO m => Handle -> Pipe WS.Event WS.Event m r
logSlack h = forever $ do
    x <- await
    liftIO $ hPutStr h (show x)
    liftIO $ hPutStr h "\n"
    yield x

--
-- Log anything that passes through this stream to a logfile
--
logReply :: MonadIO m => Handle -> Pipe (T.Text, T.Text) (T.Text, T.Text) m r
logReply h = forever $ do
    x <- await
    liftIO $ hPutStr h "\t\t\t"
    liftIO $ hPutStr h (show x)
    liftIO $ hPutStr h "\n"
    yield x

--
-- Log Irc message
--
logEvent :: MonadIO m => Handle -> Pipe (BotEvent IRC.Message) (BotEvent IRC.Message) m r
logEvent h = forever $ do
    x <- await
    case x of
        EMessage _ x' -> (do
            liftIO $ hPutStr h "\t"
            liftIO $ BS.hPutStr h (IRC.encode x')
            liftIO $ hPutStr h "\n"
            )
        _ -> return ()
    yield x

--
-- Log Irc Command message
--
logIrc :: MonadIO m => Handle -> Pipe IRC.Message IRC.Message m r
logIrc h = forever $ do
    x <- await
    liftIO $ hPutStr h "\t\t"
    liftIO $ BS.hPutStr h (IRC.encode x)
    liftIO $ hPutStr h "\n"
    yield x


--
-- Pump Message into Bot Queue
--
messagePump :: (Monad m, MonadIO m) => ServerState SlackConfig a -> Consumer (BotEvent a) m ()
messagePump ss = forever $ do
    msg <- await
    liftIO $ atomically $ writeTQueue (botQueue ss) (msg, replyQueue ss)

--
-- Vacuum, fetch replies and send to network
--
messageVacuum :: (Monad m, MonadIO m) => ServerState SlackConfig a -> Producer (BotCommand a) m ()
messageVacuum ss = forever (liftIO (atomically $ readTQueue (replyQueue ss)) >>= yield)


--
-- Emit connection established and set that we successfully connected (socket/tls level)
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionEstablished :: ServerState SlackConfig a -> IO ()
emitConnectionEstablished ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $
    writeTQueue queue (ConnectionEstablished, sq) >> writeTVar suc True

--
-- Emit connection loss only if we "successfully" connected
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionLoss :: ServerState SlackConfig a -> IO ()
emitConnectionLoss ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $ do
    success <- readTVar suc
    when success (writeTQueue queue (ConnectionLost, sq) >> writeTVar suc False)

--
-- Log Exception
-- TODO: build up a library of common utilities for wiring up this stuff
--
logException :: Handle -> C.SomeException -> IO ()
logException h (C.SomeException e) = BS.hPutStr h $ BS.concat
    [ "===========\n"
    , "Error Type: "
    , C8.pack $ show $ typeOf e -- TODO: Ascii packing
    , "\n"
    , "Error Message: "
    , C8.pack $ show e -- TODO: Ascii packing
    , "\n"
    , "===========\n"
    ]


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
