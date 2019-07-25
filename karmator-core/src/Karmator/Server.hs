{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server
    ( runServer

    , emitConnectionEstablished
    , emitConnectionLoss
    , messagePump
    , messageVacuum

    , logShow
    , logFormat
    , logException
    ) where

import Safe
import System.IO
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad.Reader
import Data.Typeable
import Prelude hiding (log, head, tail)
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as DL

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Pipes
import qualified Pipes.Network.TCP as PNT


--
-- Karmator
--
import Karmator.Types


--
-- Establish and run a server connection
--
runServer
    :: (ServerState a b c d -> IO ServerEvent)
    -> (ServerState a b c d -> IO ())
    -> d
    -> ServerConfig a
    -> TQueue (BotEvent b c, TQueue (BotCommand c))
    -> IO ()
runServer establish lose initialState sc queue = PNT.withSocketsDo $
    -- Establish the logfile
    withFile (logfile sc) AppendMode $ \l -> do
        -- Set log to be unbuffered for testing
        hSetBuffering l NoBuffering

        -- Server queue
        sq <- newTQueueIO
        suc <- newTVarIO False
        is <- newTVarIO initialState
        let ss = ServerState sc l queue sq suc is

        -- Establish connection
        forever $ do
            -- Upon exit of the establishConnection we ensure we always emit ConnectionLost
            errors <- runExceptT $ syncIO $ C.finally (establish ss) (lose ss)

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
-- Pump Message into Bot Queue
--
messagePump :: (Monad m, MonadIO m) => ServerState a b c d -> Consumer (BotEvent b c) m ()
messagePump ss = forever $ do
    msg <- await
    liftIO $ atomically $ writeTQueue (botQueue ss) (msg, replyQueue ss)

--
-- Vacuum, fetch replies and send to network
--
messageVacuum :: (Monad m, MonadIO m) => ServerState a b c d -> Producer (BotCommand c) m ()
messageVacuum ss = forever (liftIO (atomically $ readTQueue (replyQueue ss)) >>= yield)


--
-- Emit connection established and set that we successfully connected (socket/tls level)
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionEstablished :: ServerState a b c d -> IO ()
emitConnectionEstablished ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $
    writeTQueue queue (ConnectionEstablished, sq) >> writeTVar suc True

--
-- Emit connection loss only if we "successfully" connected
-- TODO: merge with ircConfig this does nothing unique with the config
--
emitConnectionLoss :: ServerState a b c d -> IO ()
emitConnectionLoss ServerState{botQueue=queue, replyQueue=sq, connectionSuccess=suc} = atomically $ do
    success <- readTVar suc
    when success (writeTQueue queue (ConnectionLost, sq) >> writeTVar suc False)

--
-- Log anything that is showable
--
logShow :: (Show a, MonadIO m) => String -> Handle -> Pipe a a m r
logShow p h = forever $ do
    x <- await
    liftIO $ hPutStr h p
    liftIO $ hPutStr h (show x)
    liftIO $ hPutStr h "\n"
    yield x

--
-- Log formatted message
--
logFormat :: MonadIO m => String -> (a -> String) -> Handle -> Pipe a a m r
logFormat p f h = forever $ do
    x <- await
    liftIO $ hPutStr h p
    liftIO $ hPutStr h (f x)
    liftIO $ hPutStr h "\n"
    yield x


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
