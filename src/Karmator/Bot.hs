{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    ( withIRC
    , addRoute
    , addServer
    ) where

import System.IO
import System.Time
import Control.Monad.Reader
import Prelude hiding (log)
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.Async

-- IRC Parser
import qualified Network.IRC as IRC

-- Karmator Stuff
import Karmator.Types
import Karmator.Route
import Karmator.Server


-- TODO: vary up the types a bit more but this will work for a start
-- TODO: fix up the type?
--withIRC :: (Monad m, MonadIO m) => BotConfig -> (BotState m -> m (BotState m)) -> m ()
withIRC :: (Monad m, MonadIO m) => BotConfig -> (BotState m -> m (BotState IO)) -> m ()
withIRC c p = do
    t <- liftIO getClockTime
    q <- liftIO newTQueueIO
    fs <- p (BotState c q [] [] t)

    liftIO $ runBot fs

addServer :: (Monad m, MonadIO m) => BotState m -> Bool -> ServerConfig -> ServerPersistentState -> m (BotState m)
addServer bs enableTls sc sps =
    return bs{servers = (enableTls, sc, sps) : servers bs}

addRoute :: (Monad m, MonadIO m) => BotState m -> Route m [CmdHandler m] -> m (BotState m)
addRoute bs r =
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

        mapM (liftIO . atomically . writeTQueue reply) (catMaybes results)
    )

executeCmdRef :: (Monad m, MonadIO m) => [CmdHandler m] -> IRC.Message -> m [Maybe IRC.Message]
executeCmdRef cs m = mapM (\(CmdRef _ st h) -> h st m) cs
