{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server.IRC
    ( runServer
    , IrcConfig
    , getServerConfig
    ) where

import Network
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

--
-- TODO: For config bits
--
import Data.ConfigFile
import Data.Set (Set)
import qualified Data.Set as Set
-- TODO: detect osx vs unix and get the cert store that way
import qualified System.X509.Unix as TLS
--import qualified System.X509.MacOS as TLS
import qualified Data.X509.Validation as TLS
--
-- Config bits above
--

-- TODO: kill this here
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PP

-- IRC Parser
import qualified Network.IRC as IRC hiding (message)
import qualified Network.IRC.Patch as IRC

-- TLS
import qualified Network.TLS as TLS
import qualified Pipes.Network.TCP.TLS as TLS
import qualified Network.Simple.TCP.TLS as TLS

import Text.Show.Functions()

-- Karmator Stuff
import Karmator.Types


--
-- Config specific bit for this server
--
data IrcConfig = IrcConfig
    { network :: String
    , server :: String
    , port :: PortNumber
    , nicks :: [BS.ByteString] -- First one then alternatives in descending order
    , userName :: BS.ByteString
    , serverPassword :: Maybe BS.ByteString

    , tlsSettings :: Maybe TLS.ClientParams

    -- Function for default encoding and decoding
    -- defaultEncoding :: BS.ByteString -> T.Text
    -- defaultDecoding :: T.Text -> BS.ByteString
    --
    --
    -- Timeouts:
    --  read - 240s
    --  connect - 10s
    --  ping_interval
    --  max_reconnect_delay
    --  delay_joins
    --
    -- Security:
    --  sasl {username, password}
    --  serverauth {username, password}
    }
    deriving (Show)

--
-- Establish and run a server connection (tls/plain)
--
runServer :: ServerConfig IrcConfig -> TQueue (BotEvent IRC.Message, TQueue (BotCommand IRC.Message)) -> IO ()
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
-- PNT.fromSocket - returns if the remote peer closes its side of the connection
-- TLS.fromContext - returns if the remote peer closes its side of the connection
--
-- NSB.sendAll - raises an exception if there was a network error (caught by syncIO)
--
establishConnection :: ServerState IrcConfig IRC.Message -> IO ServerEvent
establishConnection ss@ServerState{config=ServerConfig{serverSpecific=sc}} =
    case tlsSettings sc of
        Nothing  -> PNT.connect (server sc) (show $ port sc) $ \(sock, _)        -> handleIRC (PNT.fromSocket sock 8192) (PNT.toSocket sock) ss
        Just tls -> TLS.connect tls (server sc) (show $ port sc) $ \(context, _) -> handleIRC (TLS.fromContext context) (TLS.toContext context) ss

--
-- The IRC handler and protocol
--
handleIRC :: Producer BS.ByteString IO () -> Consumer BS.ByteString IO () -> ServerState IrcConfig IRC.Message -> IO ServerEvent
handleIRC recv send ss@ServerState{config=ssc@ServerConfig{serverSpecific=sc}, logStream=l} = do
    -- Emit connection established here
    emitConnectionEstablished ss

    -- Log irc messages in/out?
    let recv' = if logMsg ssc then recv >-> log l else recv
    let send' = if logMsg ssc then log l >-> send else send

    -- Initial connection
    -- TODO: Improve error handling if auth has failed
    runEffect (handshake ss >-> showMessage >-> send')

    -- Regular irc streaming
    loser <- race
        (runEffect (ircParserErrorLogging (network sc) (logStream ss) recv' >-> messagePump ss))
        (runEffect (messageVacuum ss >-> onlyMessages >-> showMessage >-> send'))

    -- Identify who terminated first (the send or the recv)
    return $ case loser of
        Left _  -> RecvLost
        Right _ -> SendLost

--
-- Parses incoming irc messages and emits any errors to a log and keep going
--
ircParserErrorLogging :: MonadIO m => String -> Handle -> Producer BS.ByteString m () -> Producer (BotEvent IRC.Message) m ()
ircParserErrorLogging network' l producer = do
    (result, rest) <- lift $ runStateT (PA.parse IRC.message) producer

    -- TODO: figure out how to identify a half-closed connect (zero length return value)
    -- Seems like both producer returns if the remote peer closes its side of the connections
    -- Check what attoparsec does
    --
    -- TODO: Verify that throwing an exception is the right thing here, for re-connecting
    case result of
        Nothing -> liftIO $ BS.hPutStr l "Pipe is exhausted (connection was closed)\n"
        Just r  -> do
            case r of
                Right m -> yield (EMessage network' m)
                Left e  -> liftIO $ logParsingException l e

            -- Keep going after we've either yielded or logged the error
            ircParserErrorLogging network' l rest

--
-- Handshake for initial connection to the network.
--
-- TODO: move this into an Auth Route for handling more complicated
-- handshake sequence.
--
handshake :: Monad m => ServerState IrcConfig a -> Producer IRC.Message m ()
handshake ServerState{config=ServerConfig{serverSpecific=sc}} = do
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
-- TODO: Identify if its a disconnect message and specifically disconnect
--
onlyMessages :: Monad m => Pipe (BotCommand IRC.Message) IRC.Message m ()
onlyMessages = forever $ do
    c <- await
    case c of
        CMessage m -> yield m
        _          -> return ()

--
-- Pump Message into Bot Queue
--
messagePump :: (Monad m, MonadIO m) => ServerState IrcConfig a -> Consumer (BotEvent a) m ()
messagePump ss = forever $ do
    msg <- await
    liftIO $ atomically $ writeTQueue (botQueue ss) (msg, replyQueue ss)

--
-- Vacuum, fetch replies and send to network
--
messageVacuum :: (Monad m, MonadIO m) => ServerState IrcConfig a -> Producer (BotCommand a) m ()
messageVacuum ss = forever (liftIO (atomically $ readTQueue (replyQueue ss)) >>= yield)

--
-- Emit connection established and set that we successfully connected (socket/tls level)
--
emitConnectionEstablished :: ServerState IrcConfig a -> IO ()
emitConnectionEstablished ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $
    writeTQueue queue (ConnectionEstablished, sq) >> writeTVar suc True

--
-- Emit connection loss only if we "successfully" connected
--
emitConnectionLoss :: ServerState IrcConfig a -> IO ()
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

--
-- Bot Config
--
getServerConfig
    :: ConfigParser
    -> String
    -> ExceptT CPError IO (ServerConfig IrcConfig, (String, [BS.ByteString], Set BS.ByteString, Int, [BS.ByteString]))
getServerConfig c s = do
    host      <- get c s "host"
    port      <- get c s "port"
    nicks     <- get c s "nicks"
    user      <- get c s "user"
    pass      <- get c s "pass"
    channel   <- get c s "channel" :: ExceptT CPError IO [BS.ByteString] -- Mandatory channels per host
    chan_bl   <- get c s "channel_blacklist" :: ExceptT CPError IO [BS.ByteString] -- Channel blacklist per host
    chan_join <- get c s "channel_joins" :: ExceptT CPError IO Int
    tlsHost   <- get c s "tls_host"
    tlsHash   <- get c s "tls_fingerprint" -- Hex sha256
    logfile   <- get c s "logfile"
    logirc    <- get c s "logirc"
    reconn    <- get c s "reconn"
    reWait    <- get c s "reconn_wait" -- In seconds

    tls <- case tlsHost of
        Nothing -> return Nothing
        Just th -> do
            -- Setup the TLS configuration
            tls <- liftIO $ TLS.makeClientSettings Nothing host (show port) True <$> TLS.getSystemCertificateStore
            let tls' = tls
                    { TLS.clientServerIdentification = (th, C8.pack $ show port)
                    , TLS.clientHooks = (TLS.clientHooks tls)
                        { TLS.onCertificateRequest = \_ -> return Nothing
                        }
                    }

            case tlsHash of
                Nothing    -> return $ Just tls'
                Just thash -> do
                    -- Setup hash
                    let unpackedHash = fst $ B16.decode $ C8.pack thash
                    let sid = (th, C8.pack $ show port)
                    let cache = TLS.exceptionValidationCache [(sid, TLS.Fingerprint unpackedHash)]

                    return $ Just $ tls'
                        { TLS.clientShared = (TLS.clientShared tls')
                            { TLS.sharedValidationCache = cache
                            }
                        }

    let network = DL.dropWhile ('.' ==) s
    let config = IrcConfig network host (fromInteger port) nicks user pass tls
    return (ServerConfig config reconn (reWait * 1000000) logfile logirc, (network, channel, Set.fromList chan_bl, chan_join, nicks))
