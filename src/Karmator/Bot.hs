{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    ( withIRC
    , addRoute
    , addServer
    ) where

import Data.List
import System.IO
import System.Time
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Prelude hiding (log)
import Data.Maybe
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
-- TODO: upstream the Patch
import Karmator.Types
import Karmator.Route
import Network.IRC.Patch
import Karmator.Server


-- TODO: vary up the types a bit more but this will work for a start
-- TODO: fix up the type?
--withIRC :: (Monad m, MonadIO m) => BotConfig -> (BotState m -> m (BotState m)) -> m ()
withIRC :: (Monad m, MonadIO m) => BotConfig -> (BotState m -> m (BotState IO)) -> m ()
withIRC c p = do
    t <- liftIO $ getClockTime
    q <- liftIO $ newTQueueIO
    fs <- p (BotState c q [] [] t)

    liftIO $ runBot fs

addServer :: (Monad m, MonadIO m) => BotState m -> Bool -> ServerConfig -> ServerPersistentState -> m (BotState m)
addServer bs enableTls sc sps = do
    return bs{servers = (enableTls, sc, sps) : servers bs}

addRoute :: (Monad m, MonadIO m) => BotState m -> Route m [CmdHandler m] -> m (BotState m)
addRoute bs r = do
    return bs{routes = r : routes bs}

-- TODO: this function is a tad complicated for now till we can refine things
-- TODO: fix up the type?
runBot :: BotState IO -> IO ()
runBot bs = do
    -- Start up the bot command + route
    bot <- async (runCommand (serverQueue bs) (routes bs))

    -- Give the input to each server thread and spawn them
    servers <- mapM (\(tls, sc, sps) -> liftIO $ async (runServer tls sc sps (serverQueue bs))) (servers bs)
    liftIO $ waitAny (bot : servers)
    return ()

runCommand :: (Monad m, MonadIO m, Functor m) => TQueue (IRC.Message, TQueue IRC.Message) -> [Route m [CmdHandler m]] -> m ()
runCommand q routes = forever $ forM routes (\route -> do
        (msg, reply) <- liftIO $ atomically $ readTQueue q

        -- TODO: Extend this to be nicer
        cmdRefs <- runRoute route msg
        results <- executeCmdRef cmdRefs msg

        mapM (\msg -> liftIO $ atomically (writeTQueue reply msg)) (catMaybes results)
    )

executeCmdRef :: (Monad m, MonadIO m) => [CmdHandler m] -> IRC.Message -> m [(Maybe IRC.Message)]
executeCmdRef cs m = mapM (\(CmdRef _ st h) -> h st m) cs
