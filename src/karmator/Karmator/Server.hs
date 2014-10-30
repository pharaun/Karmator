{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server
    ( runServer
    ) where

import Safe
import System.IO
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Data.Typeable
import Prelude hiding (log, head, tail)
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS

-- TODO: kill this here
import qualified Data.ByteString.Char8 as C8

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PP

-- IRC Parser
import qualified Network.IRC as IRC hiding (message)
import qualified Network.IRC.Patch as IRC

-- TLS
import qualified Pipes.Network.TCP.TLS as TLS
import qualified Network.Simple.TCP.TLS as TLS

-- Karmator Stuff
import Karmator.Types

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
                Left e  -> logException l e

            -- Wait for a bit before retrying
            threadDelay $ reconnectWait sc

--
-- Establish the connection
--
establishConnection :: ServerState -> IO ()
establishConnection ss@ServerState{config=sc} =
    case tlsSettings sc of
        Nothing  -> PNT.connect (server sc) (show $ port sc) $ \(sock, _)        -> handleIRC (PNT.fromSocket sock 8192) (PNT.toSocket sock) ss
        Just tls -> TLS.connect tls (server sc) (show $ port sc) $ \(context, _) -> handleIRC (TLS.fromContext context) (TLS.toContext context) ss

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
        (runEffect (ircParserErrorLogging (network sc) (logStream ss) recv' >-> messagePump ss))
        (runEffect (messageVacuum ss >-> onlyMessages >-> showMessage >-> send'))

--
-- Parses incoming irc messages and emits any errors to a log and keep going
--
ircParserErrorLogging :: MonadIO m => String -> Handle -> Producer BS.ByteString m () -> Producer BotEvent m ()
ircParserErrorLogging network l producer = do
    (result, rest) <- lift $ runStateT (PA.parse IRC.message) producer

    case result of
        Nothing -> liftIO $ BS.hPutStr l "Pipe is exhausted (connection was closed)\n"
        Just r  -> do
            case r of
                Right m -> yield (EMessage network m)
                Left e  -> liftIO $ logParsingException l e

            -- Keep going after we've either yielded or logged the error
            ircParserErrorLogging network l rest

--
-- Handshake for initial connection to the network.
--
-- TODO: move this into an Auth Route for handling more complicated
-- handshake sequence.
--
handshake :: Monad m => ServerState -> Producer IRC.Message m ()
handshake ServerState{config=sc} = do
    let nick = headNote "Server.hs: handshake - please set atleast one nickname" $ nicks sc
    let user = userName sc
    let pasw = serverPassword sc

    -- Required for establishing a connection to irc
    case pasw of
        Just p  -> yield $ IRC.pass p
        Nothing -> return ()
    yield $ IRC.nick nick
    yield $ IRC.user nick "0" "*" user

    return ()

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
        CMessage m -> yield m
        _          -> return ()

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
messageVacuum ss = forever (liftIO (atomically $ readTQueue (replyQueue ss)) >>= yield)

--
-- Emit connection established and set that we successfully connected (socket/tls level)
--
emitConnectionEstablished :: ServerState -> IO ()
emitConnectionEstablished ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $
    writeTQueue queue (ConnectionEstablished, sq) >> writeTVar suc True

--
-- Emit connection loss only if we "successfully" connected
--
emitConnectionLoss :: ServerState -> IO ()
emitConnectionLoss ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $ do
    success <- readTVar suc
    when success (writeTQueue queue (ConnectionLost, sq) >> writeTVar suc False)

--
-- Log anything that passes through this stream to a logfile
--
log :: MonadIO m => Handle -> Pipe BS.ByteString BS.ByteString m r
log h = forever $ do
    x <- await
    liftIO $ BS.hPutStr h x
    yield x

--
-- Log Exception
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
-- Log Parsing Exception
--
logParsingException :: Handle -> PA.ParsingError -> IO ()
logParsingException h (PA.ParsingError c m) = BS.hPutStr h $ BS.concat
    [ "===========\n"
    , "Error Type: Parsing\n"
    , "Error Message: "
    , C8.pack m
    , "\n"
    , "Error Context: "
    , C8.pack $ show c
    , "\n"
    , "===========\n"
    ]
