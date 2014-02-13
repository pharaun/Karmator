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

            runEffect $ connect sc sps l sock >-> ping sock l >-> eat
        )
    )

eat :: MonadIO m => Consumer BS.ByteString m ()
eat = forever $ await

ping :: MonadIO m => Socket -> Handle -> Pipe BS.ByteString BS.ByteString m ()
ping s l = forever $ do
    msg <- await

    if "PING :" `BS.isPrefixOf` msg
    then liftIO $ runEffect $ dump (BS.concat ["PONG", " ", ":", BS.drop 6 msg, "\r\n"]) >-> log l >-> PNT.toSocket s
    else yield msg

dump :: Monad m => BS.ByteString -> Producer BS.ByteString m ()
dump i = yield i


command :: MonadIO m => Socket -> Handle -> Pipe BS.ByteString BS.ByteString m ()
command = undefined


--eval :: String -> Net ()
--eval     "!uptime"             = uptime >>= privmsg
--eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
--eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)

--    write "PRIVMSG" $ BS.concat [chan, " :", C8.pack s]

--uptime :: Net String
--uptime = do
--    now  <- io getClockTime
--    zero <- asks starttime
--    return . pretty $ diffClockTimes now zero
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
-- Right now looks like there's a couple of approaches to the issue of
-- output and passing things on.
--
-- 1. Abstract the "await" interface into an interface that supports
--      automatical forwarding of "replied" data, such as "data message = Reply a | Message b"
--      in which the irc parser stuff the server message into "Message b"
--      and when a module wants to reply it just "yield Reply a" otherwise
--      it forwards the message. Then on the "await" part it will
--      automatically filter out the replies so it only returns an
--      unprocessed Message. Would need a final stage that filters out all
--      unprocessed messages.
--
-- 2. Some form of external queue for early-exit, basically when a module
--      wants to send a message it sticks it into another concurrent queue
--      which takes care of this, and it can just omit forwarding the
--      message. The advantage of this is is it doesn't need to keep
--      forwarding the message down the chain. Disadvantage is that it
--      includes concurrency so its no longer as easy to reason about as
--      the single threaded code above.
--
-- 3. Some form of graph, in which you can emit the reply down one side,
--      and the message can be kept on passed down the other side, and this
--      could allow for more sophsicated error recovery via taking
--      different routes and being able to plug in code for dealing with
--      this.
