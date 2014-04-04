{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Network
import System.IO
import System.Time
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Prelude hiding (log)
import Data.Maybe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Prelude as PP
import Pipes

-- IRC Parser
import qualified Network.IRC as IRC
import Control.Applicative
import Data.Attoparsec.ByteString

-- TLS
import qualified Pipes.Network.TCP.TLS as TLS
import qualified Network.Simple.TCP.TLS as TLS
import qualified System.X509.Unix as TLS
import qualified Network.TLS as TLS


-- Per server config for the bot
data ServerConfig = ServerConfig
    { server :: String
    , port :: PortNumber
    , nicks :: [BS.ByteString] -- First one then alternatives in descending order
    , userName :: BS.ByteString
    , serverPassword :: Maybe BS.ByteString

    -- TODO
    , reconnect :: Bool

    , logfile :: String -- logfile

    -- Function for default encoding and decoding
--    , defaultEncoding :: BS.ByteString -> T.Text
--    , defaultDecoding :: T.Text -> BS.ByteString

    -- Rates:
    --  messages_per_seconds, server_queue_size
    --
    -- Messages:
    --  message_split_start, message_split_end, max_messages, encoding
    --
    -- Timeouts:
    --  read - 240s
    --  connect - 10s
    --  ping_interval
    --  max_reconnect_delay
    --  delay_joins
    --
    -- Security:
    --  ssl {use, verify, client_cert, ca_path}
    --  sasl {username, password}
    --  serverauth {username, password}
    --
    -- Misc Server:
    --  log-level
    --  realname
    --  modes
    --
    }


-- Persistent State:
data ServerPersistentState = ServerPersistentState
    { channels :: [BS.ByteString]
--   channels, encoding
    }

-- Ephemeral State:
data ServerState = ServerState
    { session :: ServerPersistentState
    , config :: ServerConfig
    , logStream :: Handle

    -- Debugging/initial test impl
    , startTime :: ClockTime
    }


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
        [ return . ping
        , uptime t
        , return . motdJoin
--        , quit -- TODO: need to implement a way to exit/die so that we gracefully exit from the server
        ]
        (\a -> a msg)

    -- TODO: Unsafe head
    unless (null result) (yield $ head result)

-- TODO: with the precidate logic this would just parse the host out and reply with a pong
ping :: IRC.Message -> Maybe IRC.Message
ping msg = if "PING" == IRC.msg_command msg
           then Just $ IRC.pong (head $ IRC.msg_params msg) -- TODO: Unsafe head
           else Nothing

-- TODO: with the precidate logic this would just parse the host out and reply with a pong
motdJoin :: IRC.Message -> Maybe IRC.Message
motdJoin msg = if "700" == IRC.msg_command msg
           then Just $ IRC.joinChan "#marley"
           else Nothing

-- TODO: extend the IRC.privmsg to support sending to multiple people/channels
uptime :: MonadIO m => ClockTime -> IRC.Message -> m (Maybe IRC.Message)
uptime t msg = do
    now <- liftIO $ getClockTime

    return $ if "PRIVMSG" /= IRC.msg_command msg
    then Nothing
    else if "!uptime" `BS.isPrefixOf` (head $ tail $ IRC.msg_params msg) -- TODO: unsafe head/tail
         then Just $ IRC.privmsg "#levchins_minecraft" (C8.pack $ pretty $ diffClockTimes now t)
         else Nothing

--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s





ftestConfig :: ServerConfig
ftestConfig = ServerConfig "208.80.155.68" 6697 ["levchius"] "Ghost Bot" Nothing True "test.log"

testConfig :: ServerConfig
testConfig = ServerConfig "127.0.0.1" 9999 ["levchius"] "Ghost Bot" (Just "karma") True "test.log"

testPersistent :: ServerPersistentState
testPersistent = ServerPersistentState ["#levchins_minecraft"]

main :: IO ()
main = establishTLS ftestConfig testPersistent

-- 4. Do a dumb loop, basically since you can have nested pipes should be
--      able to do an `await` to get a message, in then you loop through
--      a list of pipes and feed the message into each, and if one yields
--      up a result, this means we should abort early and proceed to send
--      the result down the pipeline to the "sending" side.  Later on can
--      probably improve things by using actual queues, as in just queue up
--      a copy of the message into each queue and spawn off a thread for
--      each queue for processing, and then have a listener that will read
--      off the reply queue and send it.




-- UPSTREAM this to Network.IRC.Parser
--------------------------------------------------------------------------
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

tokenize  :: Parser a -> Parser a
tokenize p = p >>= \x -> IRC.spaces >> return x

-- Streaming version of the IRC message parser
message :: Parser IRC.Message
message  = do
    p <- optionMaybe $ tokenize IRC.prefix
    c <- IRC.command
    ps <- many (IRC.spaces >> IRC.parameter)
    _ <- IRC.crlf
    return $ IRC.Message p c ps


mkMessage           :: BS.ByteString -> [IRC.Parameter] -> IRC.Message
mkMessage cmd params = IRC.Message Nothing cmd params

pass  :: IRC.Password -> IRC.Message
pass u = mkMessage "PASS" [u]
--------------------------------------------------------------------------