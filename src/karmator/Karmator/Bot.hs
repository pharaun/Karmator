{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    ( runBot
    , executeCmdRef
    ) where

import System.IO
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


runBot :: [ServerConfig] -> Route [CmdHandler] -> IO ()
runBot s r = do
    -- Start up the bot command + route
    q <- newTQueueIO
    bot <- async (runCommand q [r])

    -- Give the input to each server thread and spawn them
    servers <- mapM (\sc -> async (runServer sc q)) s

    -- TODO: more sophsicated logic here, we exit upon shutdown of any
    -- server/bot async
    waitAnyCancel (bot : servers)
    return ()

runCommand :: TQueue (BotEvent, TQueue BotCommand) -> [Route [CmdHandler]] -> IO ()
runCommand q routes = forever $ do
    (msg, reply) <- atomically $ readTQueue q

    cmdRefs <- runRoute (choice routes) msg
    results <- executeCmdRef cmdRefs msg

    mapM (atomically . writeTQueue reply) (catMaybes results)


executeCmdRef :: [CmdHandler] -> BotEvent -> IO [Maybe BotCommand]
executeCmdRef cs m = mapM (\(CmdRef _ st h) -> h st m) cs
