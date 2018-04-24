{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server.Slack
    ( runServer
    , SlackConfig
    , getServerConfig
    ) where

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
import Data.Monoid ((<>))
import qualified Data.Set as Set
--
-- Config bits above
--

-- TODO: kill this here
import qualified Data.ByteString.Char8 as C8

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PP

-- Slack Parser
import qualified Web.Slack as WS


-- Karmator Stuff
import Karmator.Types


--
-- Server Specific configs
--
data SlackConfig = SlackConfig
    { network :: String
    , apiToken :: String
    }
    deriving (Show)

--
-- Establish and run a server connection
--
runServer :: ServerConfig SlackConfig -> TQueue (BotEvent, TQueue BotCommand) -> IO ()
runServer sc queue = undefined
    -- Establish the logfile
    withFile (logfile sc) AppendMode $ \l -> do
        -- Set log to be unbuffered for testing
        hSetBuffering l NoBuffering

        -- Server queue
        sq <- newTQueueIO
        suc <- newTVarIO False
        let ss = ServerState sc l queue sq suc

        -- Establish connection
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
-- TODO: unknown what kind of connection options are there
--
-- slack-api - raises an exception if there was a network error (caught by syncIO)
--
establishConnection :: ServerState SlackConfig -> IO ServerEvent
establishConnection ss@ServerState{config=ServerConfig{serverSpecific=sc}} = undefined

-- TODO: build runBot from Web.Slack (feed it the needed server state in its event handler)
-- TODO: Wire up the event handler to the loop
-- TODO: Once confirm/start launch/reconn make sure to emit(
    -- Emit connection established here
    --emitConnectionEstablished ss
-- )
-- TODO: Figure out some sort of logging of data both way

--myConfig :: SlackConfig
--myConfig = SlackConfig
--        { _slackApiToken = "..." -- Specify your API token here
--        }
--
---- type SlackBot s = Event -> Slack s ()
--echoBot :: SlackBot ()
--echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
--echoBot _ = return ()
--
--main :: IO ()
--main = runBot myConfig echoBot ()




--
-- Emit connection established and set that we successfully connected (socket/tls level)
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionEstablished :: ServerState SlackConfig -> IO ()
emitConnectionEstablished ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $
    writeTQueue queue (ConnectionEstablished, sq) >> writeTVar suc True

--
-- Emit connection loss only if we "successfully" connected
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionLoss :: ServerState SlackConfig -> IO ()
emitConnectionLoss ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $ do
    success <- readTVar suc
    when success (writeTQueue queue (ConnectionLost, sq) >> writeTVar suc False)

--
-- Log Exception
-- TODO: build up a library of common utilities for wiring up this stuff
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
-- Config
--
getServerConfig
    :: ConfigParser
    -> String
    -> ExceptT CPError IO (ServerConfig SlackConfig)
getServerConfig c s = do
    apitoken  <- get c s "api_token"
    logfile   <- get c s "logfile"
    logslack  <- get c s "logslack"
    reconn    <- get c s "reconn"
    reWait    <- get c s "reconn_wait" -- In seconds

    let network = DL.dropWhile ('.' ==) s
    let config = SlackConfig network apitoken
    return $ ServerConfig config reconn (reWait * 1000000) logfile logslack
