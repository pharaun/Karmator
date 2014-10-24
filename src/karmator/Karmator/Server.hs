{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server
    ( runServer
    ) where

import Safe
import Control.Error
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Data.List hiding (head, tail)
import Prelude hiding (log, head, tail)
import System.IO
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Control.Monad.Catch as C

import qualified Data.ByteString as BS

-- TODO: kill this here
import qualified Data.ByteString.Char8 as C8

import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Prelude as PP
import Pipes

-- IRC Parser
import qualified Network.IRC as IRC
import Control.Applicative

-- TLS
import qualified Pipes.Network.TCP.TLS as TLS
import qualified Network.Simple.TCP.TLS as TLS
import qualified System.X509.Unix as TLS
import qualified Network.TLS as TLS

-- Karmator Stuff
import Karmator.Types
import Network.IRC.Patch

--
-- Establish and run a server connection (tls/plain)
--
runServer :: ServerConfig -> TQueue (BotEvent, TQueue BotCommand) -> IO ()
runServer sc queue = PNT.withSocketsDo $
    -- Establish the logfile
    withFile (logfile sc) AppendMode $ \l -> do
        -- Set log to be unbuffered for testing
        hSetBuffering l NoBuffering

        -- Server queue
        sq <- newTQueueIO
        suc <- newTVarIO False
        let ss = ServerState sc l queue sq suc

        -- Establish tls/norm connection
        forever $ do
            -- Upon exit of the establishConnection we ensure we always emit ConnectionLost
            errors <- runEitherT $ syncIO $ C.finally (establishConnection ss) (emitConnectionLoss ss)

            -- TODO: good place for tallying the number of failure and
            -- shutting the server connection off if it exceeds some
            -- threshold.
            case errors of
                Right _ -> return ()
                -- TODO: Need to identify the type of error and allow some to propagate upward - IE don't just catch all.
                Left x  -> logError l x

            -- Wait for a bit before retrying
            threadDelay $ reconnectWait sc

--
-- Establish the connection
--
establishConnection :: ServerState -> IO ()
establishConnection ss@ServerState{config=sc, logStream=l} =
    case tlsSettings sc of
        Nothing  -> PNT.connect (server sc) (show $ port sc) $ \(sock, _)        -> handleIRC (PNT.fromSocket sock 8192) (PNT.toSocket sock) ss
        Just tls -> TLS.connect tls (server sc) (show $ port sc) $ \(context, _) -> handleIRC (TLS.fromContext context) (TLS.toContext context) ss

emitConnectionEstablished :: ServerState -> IO ()
emitConnectionEstablished ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $
    writeTQueue queue (ConnectionEstablished, sq) >> writeTVar suc True

emitConnectionLoss :: ServerState -> IO ()
emitConnectionLoss ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $ do
    success <- readTVar suc
    when success (writeTQueue queue (ConnectionLost, sq) >> writeTVar suc False)





--
-- Parses incoming irc messages and emits any errors to a log and keep going
--
ircParserErrorLogging :: MonadIO m => Handle -> Producer BS.ByteString m () -> Producer BotEvent m ()
ircParserErrorLogging l producer = do
    (result, rest) <- lift $ runStateT (PA.parse message) producer

    -- TODO: error handling to handle parsing failure/etc,
    -- but that should be mostly handled inside handleIRC
    -- itself
    -- TODO: kill this here
    -- TODO: we probably want to handle parsing errors (ie log to file then
    -- keep going), but if pipe is exhausted, probably should exit the loop
    case result of
        Nothing -> liftIO $ logError l "Pipe is exhausted for irc parser\n"
        Just y  ->
            case y of
                Right x -> yield (EMessage x)
                Left x  -> liftIO $ logError l x
    ircParserErrorLogging l rest

--
-- Log error
--
logError :: (Show a) => Handle -> a -> IO ()
logError h e = BS.hPutStr h $ BS.concat
    [ "===========\n"
    , C8.pack $ show e -- TODO: Ascii packing
    , "\n"
    , "===========\n"
    ]

--
-- Format outbound IRC messages
--
showMessage :: Monad m => Pipe IRC.Message BS.ByteString m ()
showMessage = PP.map encode
    where
        encode = (`BS.append` "\r\n") . IRC.encode

--
-- Filter any non Message
--
onlyMessages :: Monad m => Pipe BotCommand IRC.Message m ()
onlyMessages = forever $ do
    c <- await
    case c of
        (CMessage m) -> yield m
        otherwise    -> return ()

--
-- Handshake for the initial connection to the network
-- TODO: Support/move this to some form of Auth plugin, need to be able to
--      generate requests and request replies, and need to be able to
--      perform it in lockstep or as callbacks
--
--  Example:
--      Should support doing `cap ls` to find out if the server support caps
--          - Should also proceed with pass/nick/user part of the connect
--              (to support servers that don't support cap)
--          - But if server supports caps it will reply back, need to request caps that we want/need
--          - If it does not it can either reply with 001 or so or ignore it.
--
--      Should support auth ping, for ex after registeration you may need to do auth-ping to join a channel
--
--      Should be able to support "STARTTLS" or -> SSL only
--
--      Also should in theory have a post setup for registering with nickserv or identifying your nick
--
handshake :: Monad m => ServerState -> Producer IRC.Message m ()
handshake ss = do
    let sc  = config ss

    let nick = headNote "Server.hs: handshake - please set atleast one nickname" $ nicks sc
    let chan = headNote "Server.hs: handshake - please set atleast one channel" $ channels sc
    let user = userName sc
    let pasw = serverPassword sc

    -- Required for establishing a connection to irc
    case pasw of
        Just x  -> yield $ pass x
        Nothing -> return ()
    yield $ IRC.nick nick
    yield $ IRC.user nick "0" "*" user

    -- TODO: add support for "auth ping/pong" before registering/joining channels

    -- Setup the channels
    yield $ IRC.joinChan chan

    return ()

--
-- Log anything that passes through this stream to a logfile
--
log :: MonadIO m => Handle -> Pipe BS.ByteString BS.ByteString m r
log h = forever $ do
    x <- await
    liftIO $ BS.hPutStr h x
    yield x

--
-- The IRC handler and protocol
--
handleIRC :: Producer BS.ByteString IO () -> Consumer BS.ByteString IO () -> ServerState -> IO ()
handleIRC recv send ss@ServerState{config=sc, logStream=l} = do
    -- Emit connection established here
    emitConnectionEstablished ss

    -- Log irc messages in/out?
    let recv' = if logIrc sc then recv >-> log l else recv
    let send' = if logIrc sc then log l >-> send else send

    -- Initial connection
    -- TODO: Improve error handling if auth has failed
    runEffect (handshake ss >-> showMessage >-> send')

    -- Regular irc streaming
    race_
        (runEffect (ircParserErrorLogging (logStream ss) recv' >-> messagePump ss))
        (runEffect (messageVacuum ss >-> onlyMessages >-> showMessage >-> send'))

--
-- Pump Message into Bot Queue
--
messagePump :: (Monad m, MonadIO m) => ServerState -> Consumer BotEvent m ()
messagePump ss = forever $ do
    msg <- await
    liftIO $ atomically $ writeTQueue (botQueue ss) (msg, replyQueue ss)

--
-- Vacuum, fetch replies and send to network
--
messageVacuum :: (Monad m, MonadIO m) => ServerState -> Producer BotCommand m ()
messageVacuum ss = forever ((liftIO $ atomically $ readTQueue (replyQueue ss)) >>= yield)
