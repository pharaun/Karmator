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
import qualified Data.ByteString as B
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB


server = "irc.freenode.org"
port   = 6667
chan   = "#levchins_minecraft"
nick   = "levicus"
log    = "dump.log"

--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Net = ReaderT Bot IO
data Bot = Bot
    { socket :: Handle
    , logFile :: Handle
    , starttime :: ClockTime
    }

--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect b = (hClose $ socket b) >> (hClose $ logFile b)
    loop st      = runReaderT run st

--
-- Connect to the server and return the initial bot state
--
connect :: IO Bot
connect = do
    t <- getClockTime

    -- Set up the hints to prefer ipv4 udp datagrams
    let myAddr = NS.defaultHints {
        NS.addrFamily = NS.AF_INET
        , NS.addrSocketType = NS.Stream
        }

    -- Looks up hostname and port then grab the first
    addrInfo <- head <$> NS.getAddrInfo (Just myAddr) (Just server) (Just $ show port)

    -- Establish a socket for communication
    sock <- NS.socket (NS.addrFamily addrInfo) NS.Stream NS.defaultProtocol

    -- Connect
    NS.connect sock (NS.addrAddress addrInfo)

    -- Convert to a handle
    h <- NS.socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering

    -- Logfile, write only/append only
    l <- openFile log AppendMode
    hSetBuffering l NoBuffering

    return (Bot h l t)


--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :ghost bot")
    write "JOIN" chan
    asks socket >>= listen

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
    pong x    = write "PONG" (':' : drop 6 x)

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
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    l <- asks logFile

    -- Send to network and logfile
    io $ hPrintf h "%s %s\r\n" s t
    io $ hPrintf l "%s %s\r\n" s t

    -- Console output
    io $ printf    "> %s %s\n" s t

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
