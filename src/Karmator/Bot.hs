{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    -- TODO: Merge these two and add in ssl configuration
    ( establishTLS
    , establish

    ) where

import Data.List
import System.IO
import System.Time
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Prelude hiding (log)
import Data.Maybe

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
-- TODO: upstream the Patch
import Karmator.Types
import Network.IRC.Patch

-- Plugins
-- TODO: migrate these to Main.hs
import Plugins.Ping


--
-- Establish TLS connection
--
establishTLS :: ServerConfig -> ServerPersistentState -> IO ()
establishTLS sc sps = PNT.withSocketsDo $
    -- Establish the logfile
    withFile (logfile sc) AppendMode (\l -> do

        -- Establish the client configuration
        params <- TLS.makeClientSettings Nothing (server sc) (show $ port sc) True <$> TLS.getSystemCertificateStore

        -- Workaround with a bug about connecting to sockets with domain name with ipv6 bridge up
        let params' = params
                { TLS.clientServerIdentification = ("chat.freenode.net", C8.pack $ show $ port sc)
                , TLS.clientHooks = (TLS.clientHooks params)
                    { TLS.onCertificateRequest = \_ -> return $ Nothing
                    }
                }

        -- Stablish connection
        TLS.connect params' (server sc) (show $ port sc) (\(context, _) -> do

            -- Set log to be unbuffered for testing
            hSetBuffering l NoBuffering

            -- Session start time
            -- TODO: move this into the uptime plugin
            t <- getClockTime

            handleIRC (TLS.fromContext context >-> log l) (log l >-> TLS.toContext context) (ServerState sps sc l t)

            return ()
            )
        )


--
-- Establish an irc session with this server
--
establish :: ServerConfig -> ServerPersistentState -> IO ()
establish sc sps = PNT.withSocketsDo $
    -- Establish the logfile
    withFile (logfile sc) AppendMode (\l ->
        -- TODO: do some form of dns lookup and prefer either v6 or v4

        -- Establish stocket
        PNT.connect (server sc) (show $ port sc) (\(sock, _) -> do

            -- Set log to be unbuffered for testing
            hSetBuffering l NoBuffering

            -- Session start time
            -- TODO: move this into the uptime plugin
            t <- getClockTime

            handleIRC (PNT.fromSocket sock 8192 >-> log l) (log l >-> PNT.toSocket sock) (ServerState sps sc l t)

            return ()
        )
    )

--
-- The IRC handler and protocol
--
handleIRC :: (Monad m, MonadIO m) => Producer BS.ByteString m () -> Consumer BS.ByteString m () -> ServerState -> m ()
handleIRC recv send ss = do
    -- Initial connection
    -- TODO: Improve error handling if auth has failed
    runEffect $ handshake ss >-> showMessage >-> send

    -- Regular irc streaming
    runEffect $ (ircParserErrorLogging (logStream ss) recv) >-> command (startTime ss) >-> showMessage >-> send

    return ()

--
-- Parses incoming irc messages and emits any errors to a log and keep going
--
ircParserErrorLogging :: MonadIO m => Handle -> Producer BS.ByteString m () -> Producer IRC.Message m ()
ircParserErrorLogging l producer = do
    (result, rest) <- lift $ runStateT (PA.parse message) producer

    case result of
        Nothing -> liftIO $ BS.hPutStr l "Pipe is exhausted for irc parser\n"
        Just y  ->
            case y of
                Right x -> yield x
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
    let sps = session ss

    let nick = head $ nicks sc -- TODO: unsafe head
    let chan = head $ channels sps -- TODO: unsafe head
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
-- take the inbound message, loop it through a list of
-- functions/pipe/whatever
--
-- Then for early exit take the first one that response and send it on
-- downstream via yield
--
-- TODO: implement a form or precidate logic such as "and [privmsg, or [ user "xyz", server "xy" ] ] -> command
-- TODO: implement some form of infrastructure for state tracking for stateful stuff like "invite to new channel, want to stay there, leave, etc...
--
command :: (Monad m, MonadIO m) => ClockTime -> Pipe IRC.Message IRC.Message m ()
command t = forever $ do
    msg <- await

    -- TODO: look into seeing if there's a way to setup some form of
    -- generic filtering rules such as "if chan = y send to x", etc..
    -- Perhaps Pipe.Prelude.Filter
    result <- catMaybes <$> forM
        [ \a -> return $ if pingMatch a then ping a else Nothing
        , \a -> return $ if motdMatch a then motdJoin a else Nothing
        , \a -> if uptimeMatch a then uptime t a else return $ Nothing -- IO

--        , quit -- TODO: need to implement a way to exit/die so that we gracefully exit from the server
        ]
        (\a -> a msg)

    -- TODO: Unsafe head
    unless (null result) (yield $ head result)
