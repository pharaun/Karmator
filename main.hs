{-# LANGUAGE OverloadedStrings, Rank2Types #-}
import Data.List
import Network
import System.IO
import System.Time
import System.Exit
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Control.Exception
import Text.Printf
import Prelude hiding (catch, log)
import Data.Maybe

import Data.Functor ((<$>))
import Data.List (head)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import qualified Pipes.ByteString as PBS
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Lift as Lift
import Pipes

-- IRC Parser
import qualified Network.IRC as IRC

-- test
import Control.Monad.Trans.Error

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
    { session :: ServerPersistentState
    , config :: ServerConfig

    -- Debugging/initial test impl
    , starttime :: ClockTime
    }

--
-- Establish an irc session with this server
-- TODO: provide the listeners/stuff needed to run the inner irc controller
--
establish :: ServerConfig -> ServerPersistentState -> IO ()
establish sc sps = PNT.withSocketsDo $
    -- Establish the logfile
    withFile "test.log" AppendMode (\l ->
        -- TODO: do some form of dns lookup and prefer either v6 or v4
        -- Establish stocket
        PNT.connect (server sc) (show $ port sc) (\(sock, _) -> do
            -- Session start time
            t <- getClockTime

            -- Set log to be unbuffered for testing
            hSetBuffering l NoBuffering

            -- Initial connection
            runEffect $ connect sc sps l sock

            -- Rest of the program
            runEffect $ Lift.runErrorP $ errorLog (PNT.fromSocket sock 8192 >-> log l) >-> command t >-> log l >-> PNT.toSocket sock

            return ()
        )
    )

-- TODO: Getting what seems to be spurrious ParsingError "endOfInput"
-- This is failing to pass the parse on...
errorLog :: MonadIO m => Producer BS.ByteString m () -> Producer IRC.Message (ErrorT (PA.ParsingError, Producer C8.ByteString m ()) m) ()
errorLog producer = do
--    (result, leftover) <- runStateT (PA.parse IRC.message) producer
    
    (Lift.errorP $ PA.parsed IRC.message $ producer) `Lift.catchError` \(err, str) -> do
        liftIO (putStrLn "===========")
        liftIO (putStrLn $ show err)
        liftIO (putStrLn "===========")
        errorLog producer

-- Initial connect attempt
connect :: MonadIO m => ServerConfig -> ServerPersistentState -> Handle -> Socket -> Effect m ()
connect sc sps l socket = handshake sc sps >-> log l >-> PNT.toSocket socket

--
-- Handshake for the initial connection to the network
--
handshake :: Monad m => ServerConfig -> ServerPersistentState -> Producer BS.ByteString m ()
handshake sc sps = do
    let nick = head $ nicks sc -- TODO: unsafe head
    let chan = head $ channels sps -- TODO: unsafe head

    -- We skip reading in anything and start dumping back out.
    yield $ BS.concat ["NICK", " ", nick, "\r\n"]
    yield $ BS.concat ["USER", " ", (nick `BS.append` " 0 * :ghost bot"), "\r\n"]
    yield $ BS.concat ["JOIN", " ", chan, "\r\n"]

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
command :: (Monad m, MonadIO m) => ClockTime -> Pipe IRC.Message BS.ByteString m ()
command t = forever $ do
    msg <- await


    liftIO (putStrLn "===========")
    liftIO (putStrLn $ show msg)
    liftIO (putStrLn "===========")


    -- TODO: look into seeing if there's a way to setup some form of
    -- generic filtering rules such as "if chan = y send to x", etc..
    -- Perhaps Pipe.Prelude.Filter
    result <- catMaybes <$> forM
        [ return . ping
        , uptime t
--        , quit
        ]
        (\a -> a msg)

    unless (null result) (yield $ head result)

ping :: IRC.Message -> Maybe BS.ByteString
ping msg = if "PING" == IRC.msg_command msg
           then Just $ BS.concat ["PONG", " ", ":", head $ IRC.msg_params msg, "\r\n"] -- TODO: Unsafe head
           else Nothing

quit :: MonadIO m => IRC.Message -> m (Maybe BS.ByteString)
quit = undefined

uptime :: MonadIO m => ClockTime -> IRC.Message -> m (Maybe BS.ByteString)
uptime t msg = do
    now <- liftIO getClockTime

    return $ if "!uptime" `BS.isPrefixOf` (head $ tail $ IRC.msg_params msg) -- TODO: unsafe head/tail
             then Just $ BS.concat ["PRIVMSG", " ", "#levchins_minecraft", " ", ":", (C8.pack $ pretty $ diffClockTimes now t), "\r\n"]
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

--
-- Format outbound IRC messages
--
showMessage :: Monad m => Pipe IRC.Message BS.ByteString m ()
showMessage = undefined -- IRC.showMessage



testConfig :: ServerConfig
--testConfig = ServerConfig "irc.freenode.net" 6667 ["levchius"] True
--testConfig = ServerConfig "86.65.39.15" 6667 ["levchius"] True
testConfig = ServerConfig "127.0.0.1" 9999 ["levchius"] True

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
--
-- 4. Do a dumb loop, basically since you can have nested pipes should be
--      able to do an `await` to get a message, in then you loop through
--      a list of pipes and feed the message into each, and if one yields
--      up a result, this means we should abort early and proceed to send
--      the result down the pipeline to the "sending" side.  Later on can
--      probably improve things by using actual queues, as in just queue up
--      a copy of the message into each queue and spawn off a thread for
--      each queue for processing, and then have a listener that will read
--      off the reply queue and send it.
