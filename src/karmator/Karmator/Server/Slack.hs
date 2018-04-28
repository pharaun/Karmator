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
import Control.Monad.Trans.State.Strict hiding (get)
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
import Data.Set (Set)
import Data.Monoid ((<>))
import qualified Data.Set as Set
--
-- Config bits above
--

-- TODO: kill this here
import qualified Data.ByteString.Char8 as C8

import Pipes
import Pipes.Prelude (drain)
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PP

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
runServer :: ServerConfig SlackConfig -> TQueue (BotEvent, TQueue BotCommand) -> IO ()
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
establishConnection :: ServerState SlackConfig -> IO ServerEvent
establishConnection ss@ServerState{config=ServerConfig{serverSpecific=sc}} = do
    let conf = WS.SlackConfig{ WS._slackApiToken = apiToken sc}

    -- TODO: need some sort of MVAR set here when the pipeBot part fails
    -- that we can then read and use to Emit RecvLost or SendLost
    WS.withSlackHandle conf (pipeBot ss)

    return RecvLost

-- TODO: build runBot from Web.Slack (feed it the needed server state in its event handler)
-- TODO: Wire up the event handler to the loop
-- TODO: Once confirm/start launch/reconn make sure to emit(
    -- Emit connection established here
    --emitConnectionEstablished ss
-- )
-- TODO: Figure out some sort of logging of data both way
pipeBot :: ServerState SlackConfig -> WS.SlackHandle -> IO ()
pipeBot ss@ServerState{config=ssc, logStream=l} h = do
    emitConnectionEstablished ss

    -- Start bot streaming
    runEffect $ slackProducer h >-> (log l) >-> slackToIrc >-> (logIrc l) >-> drain

    return ()


--
-- Pump messages from slack into pipe streams
--
slackProducer :: WS.SlackHandle -> Producer WS.Event IO ()
slackProducer h = forever $ lift (WS.getNextEvent h) >>= yield


--pipeBot :: SlackHandle -> IO ()
--pipeBot h = runEffect $ slackProducer h >-> slackConsumer h
--
--slackProducer :: SlackHandle -> Producer Event IO ()
--slackProducer h = forever $ lift (getNextEvent h) >>= yield
--
--slackConsumer :: SlackHandle -> Consumer Event IO ()
--slackConsumer h = forever $
--    await >>= \case
--        (Message cid _ msg _ _ _) -> lift $ sendMessage h cid msg
--        _ -> return ()

--handleIRC :: Producer BS.ByteString IO () -> Consumer BS.ByteString IO () -> ServerState IrcConfig -> IO ServerEvent
--handleIRC recv send ss@ServerState{config=ssc@ServerConfig{serverSpecific=sc}, logStream=l} = do
--    -- Emit connection established here
--    emitConnectionEstablished ss
--
--    -- Log irc messages in/out?
--    let recv' = if logMsg ssc then recv >-> log l else recv
--    let send' = if logMsg ssc then log l >-> send else send
--
--    -- Regular irc streaming
--    loser <- race
--        (runEffect (ircParserErrorLogging (network sc) (logStream ss) recv' >-> messagePump ss))
--        (runEffect (messageVacuum ss >-> onlyMessages >-> showMessage >-> send'))


--
-- Transform Slack events into Irc messages to pump it
--
slackToIrc :: MonadIO m => Pipe WS.Event IRC.Message m r
slackToIrc = forever $ do
    e <- await

    -- TODO: handle BotIds
    -- TODO: see a way to remap the <@userid> -> an actual user name
    -- TODO: find a good way to handle remapping the user-id+channel id to an actual name (May need some new columns)
    -- TODO: Going to need to find another new message type to pump the id stuff through (hacky might be enough) then
    --      a new hook to feed those id updates/info into the database that can be used for lookups
    case e of
        WS.Message (WS.Id {WS._getId = cid}) (WS.UserComment (WS.Id {WS._getId = uid})) msg ts _ _ -> (do
            -- All fields are Text, get a nice converter to pack into utf8 bytestring
            let prefix = IRC.NickName (TE.encodeUtf8 uid) (Just (TE.encodeUtf8 uid)) (Just (C8.pack "SlackServer"))
            let message = IRC.Message (Just prefix) (C8.pack "PRIVMSG") [TE.encodeUtf8 cid, TE.encodeUtf8 msg]

            --Right m -> yield (EMessage network' m)
            yield message
            )
        _ -> return ()


--
-- Log anything that passes through this stream to a logfile
--
log :: MonadIO m => Handle -> Pipe WS.Event WS.Event m r
log h = forever $ do
    x <- await
    liftIO $ hPutStr h (show x)
    liftIO $ hPutStr h "\n"
    yield x

--
-- Log Irc message
--
logIrc :: MonadIO m => Handle -> Pipe IRC.Message IRC.Message m r
logIrc h = forever $ do
    x <- await
    liftIO $ hPutStr h "\t"
    liftIO $ BS.hPutStr h (IRC.encode x)
    yield x


--
-- Pump Message into Bot Queue
--
messagePump :: (Monad m, MonadIO m) => ServerState SlackConfig -> Consumer BotEvent m ()
messagePump ss = forever $ do
    msg <- await
    liftIO $ atomically $ writeTQueue (botQueue ss) (msg, replyQueue ss)

--
-- Vacuum, fetch replies and send to network
--
messageVacuum :: (Monad m, MonadIO m) => ServerState SlackConfig -> Producer BotCommand m ()
messageVacuum ss = forever (liftIO (atomically $ readTQueue (replyQueue ss)) >>= yield)


--
-- Emit connection established and set that we successfully connected (socket/tls level)
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionEstablished :: ServerState SlackConfig -> IO ()
emitConnectionEstablished ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $
    writeTQueue queue (ConnectionEstablished, sq) >> writeTVar suc True

--
-- Emit connection loss only if we "successfully" connected
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionLoss :: ServerState SlackConfig -> IO ()
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
