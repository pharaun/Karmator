{-# LANGUAGE OverloadedStrings, Rank2Types #-}
import Data.List
import Network
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch, log)

import Data.Functor ((<$>))
import Data.List (head)
import Network.BSD (HostName)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T


import qualified Network.Simple.TCP as NST
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.ByteString as PBS
import qualified Pipes.Attoparsec as PA
import Pipes


-- Per server config for the bot
data ServerConfig = ServerConfig
    { server :: String
    , port :: Int
    , nicks :: [BS.ByteString] -- First one then alternatives in descending order
    , reconnect :: Bool

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

-- State TODO:
--  initial channel to join
--  honor invitation/request to join other channels or not/leaves, etc
--
--  channels:
--   channels, encoding

-- Persistent State:
data ServerPersistentState = ServerPersistentState
    { channels :: [BS.ByteString]
    }

-- Ephemeral State:
data ServerState = ServerState
    { socket :: Handle

    -- Other stuff
    , session :: ServerPersistentState
    , config :: ServerConfig

    -- Debugging/initial test impl
    , starttime :: ClockTime
    , logFile :: Handle
    }


data ServerStatePipes = ServerStatePipes
    { sessionP :: ServerPersistentState
    , configP :: ServerConfig

    -- Debugging/initial test impl
    , starttimeP :: ClockTime
    }

--
-- Establish an irc session with this server
-- TODO: provide the listeners/stuff needed to run the inner irc controller
--
establish :: ServerConfig -> ServerPersistentState -> IO ()
establish sc sps = NST.withSocketsDo $
    -- Establish the logfile
    withFile "test.log" AppendMode (\l ->
        -- TODO: do some form of dns lookup and prefer either v6 or v4
        -- Establish stocket
        NST.connect (server sc) (show $ port sc) (\(sock, _) -> do
            -- Session start time
            t <- getClockTime

            -- Set log to be unbuffered for testing
            hSetBuffering l NoBuffering


            runEffect $ connect sc sps l sock >-> eat
        )
    )

eat :: MonadIO m => Consumer BS.ByteString m ()
eat = forever $ await

connect :: MonadIO m => ServerConfig -> ServerPersistentState -> Handle -> Socket -> Producer BS.ByteString m ()
connect sc sps l socket = runEffect (PNT.fromSocket socket 4096 >-> log l >-> handshake sc sps >-> log l >-> PNT.toSocket socket)

--
-- Handshake for the initial connection to the network
--
handshake :: MonadIO m => ServerConfig -> ServerPersistentState -> Pipe BS.ByteString BS.ByteString (Producer BS.ByteString m) r
handshake sc sps = do
    let nick = head $ nicks sc -- TODO: unsafe head
    let chan = head $ channels sps -- TODO: unsafe head

    -- We skip reading in anything and start dumping back out.
    yield $ BS.concat ["NICK", " ", nick, "\r\n"]
    yield $ BS.concat ["USER", " ", (nick `BS.append` " 0 * :ghost bot"), "\r\n"]
    yield $ BS.concat ["JOIN", " ", chan, "\r\n"]

    -- We are now done handshaking, forever forward bytes
    for cat (lift . yield) -- forever (await >>= lift . yield)

--
-- Log anything that passes through this stream to a logfile
--
log :: MonadIO m => Handle -> Pipe BS.ByteString BS.ByteString m r
log h = forever $ do
    x <- await
    liftIO $ BS.hPutStr h x
    yield x



testConfig :: ServerConfig
--testConfig = ServerConfig "irc.freenode.net" 6667 ["levchius"] True
testConfig = ServerConfig "86.65.39.15" 6667 ["levchius"] True

testPersistent :: ServerPersistentState
testPersistent = ServerPersistentState ["#levchins_minecraft"]

main :: IO ()
main = establish testConfig testPersistent











--
-- Connect to the server and return the initial bot state
--
connectToServer :: ServerConfig -> ServerPersistentState -> IO ServerState
connectToServer sc sps = do
    t <- getClockTime

    -- Set up the hints to prefer ipv4 udp datagrams
    let myAddr = NS.defaultHints {
        NS.addrFamily = NS.AF_INET
        , NS.addrSocketType = NS.Stream
        }

    -- Looks up hostname and port then grab the first
    addrInfo <- head <$> NS.getAddrInfo (Just myAddr) (Just $ server sc) (Just $ show $ port sc)

    -- Establish a socket for communication
    sock <- NS.socket (NS.addrFamily addrInfo) NS.Stream NS.defaultProtocol

    -- Connect
    NS.connect sock (NS.addrAddress addrInfo)

    -- Convert to a handle
    h <- NS.socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering

    -- Logfile, write only/append only
    l <- openFile "test.log" AppendMode
    hSetBuffering l NoBuffering

    return $ ServerState h sps sc t l


type Net = ReaderT ServerState IO

--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
runServerLoop :: Net ()
runServerLoop = do
    config <- asks config
    state <- asks session

    let nick = head $ nicks config -- TODO: unsafe head
    let chan = head $ channels state -- TODO: unsafe head

    l <- asks logFile

    write "NICK" nick
    write "USER" (nick `BS.append` " 0 * :ghost bot")
    write "JOIN" chan

    -- Go on to the loop
    asks socket >>= listen


--
-- Send a message out to the server we're currently connected to
--
write :: BS.ByteString -> BS.ByteString -> Net ()
write s t = do
    h <- asks socket
    l <- asks logFile

    -- TODO: fix this to pull the encoding from the channel/server
    -- Send to network and logfile
    io $ C8.hPut h $ BS.concat [s, " ", t, "\r\n"]
    io $ C8.hPut l $ BS.concat [s, " ", t, "\r\n"]

    -- Console output
    io $ C8.putStr $ BS.concat [s, " ", t, "\n"]

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    l <- asks logFile

    -- Dump to logfile and then console
    io $ hPrintf l "%s\r\n" s
    io (putStrLn s)


    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" $ C8.pack (':' : drop 6 x)

--
-- Dispatch a command
--
eval :: String -> Net ()
eval     "!uptime"             = uptime >>= privmsg
eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval     _                     = return () -- ignore everything else

--
-- Send a privmsg to the current chan + server
--
privmsg :: String -> Net ()
privmsg s = do
    -- TODO: This is doing it wrong
    state <- asks session
    let chan = head $ channels state -- TODO: unsafe head

    write "PRIVMSG" $ BS.concat [chan, " :", C8.pack s]


--
-- Calculate and pretty print the uptime
--
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

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

--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO
