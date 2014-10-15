{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server
    ( runServer
    ) where

import Data.List
import System.IO
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Prelude hiding (log)
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Concurrent.Async

import qualified Data.ByteString as BS
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
    withFile (logfile sc) AppendMode (\l -> do
        -- Set log to be unbuffered for testing
        hSetBuffering l NoBuffering

        -- Server queue
        sq <- newTQueueIO
        let ss = ServerState sc l queue sq

        -- Establish tls/norm connection
        forever $ do
            -- TODO: dedup this further
            case (tlsSettings sc) of
                Nothing  -> PNT.connect (server sc) (show $ port sc) (\(sock, _) -> do
                        -- Emit connection established here
                        atomically $ writeTQueue queue (ConnectionEstablished, sq)

                        -- Normal irc stuff
                        handleIRC (PNT.fromSocket sock 8192 >-> log l) (log l >-> PNT.toSocket sock) ss
                    )

                Just tls -> TLS.connect tls (server sc) (show $ port sc) (\(context, _) -> do
                        -- Emit connection established here
                        atomically $ writeTQueue queue (ConnectionEstablished, sq)

                        -- Normal irc stuff
                        handleIRC (TLS.fromContext context >-> log l) (log l >-> TLS.toContext context) ss
                    )

            -- Emit connection lost here
            atomically $ writeTQueue queue (ConnectionLost, sq)

            -- Wait for a bit before retrying
            threadDelay $ reconnectWait sc
        )

--
-- Parses incoming irc messages and emits any errors to a log and keep going
--
ircParserErrorLogging :: MonadIO m => Handle -> Producer BS.ByteString m () -> Producer BotEvent m ()
ircParserErrorLogging l producer = do
    (result, rest) <- lift $ runStateT (PA.parse message) producer

    case result of
        Nothing -> liftIO $ BS.hPutStr l "Pipe is exhausted for irc parser\n"
        Just y  ->
            case y of
                Right x -> yield (EMessage x)
                Left x  -> liftIO $ BS.hPutStr l $ BS.concat
                    [ "===========\n"
                    , "\n"
                    , C8.pack $ show x -- TODO: Ascii packing
                    , "\n"
                    , "===========\n"
                    ]
    ircParserErrorLogging l rest

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

    let nick = head $ nicks sc -- TODO: unsafe head
    let chan = head $ channels sc -- TODO: unsafe head
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
handleIRC recv send ss = do
    -- Initial connection
    -- TODO: Improve error handling if auth has failed
    runEffect (handshake ss >-> showMessage >-> send)

    -- Regular irc streaming
    race_
        (runEffect ((ircParserErrorLogging (logStream ss) recv) >-> messagePump ss))
        (runEffect (messageVacuum ss >-> onlyMessages >-> showMessage >-> send))

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
messageVacuum ss = forever $ do
    msg <- liftIO $ atomically $ readTQueue (replyQueue ss)
    yield msg
