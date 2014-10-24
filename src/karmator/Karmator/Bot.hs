{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    ( runBot
    , executeCmdRef
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Maybe
import Prelude hiding (log)

-- Karmator Stuff
import Karmator.Route
import Karmator.Server
import Karmator.Types


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

--
-- Execute the list of commands
--
-- TODO: Improve error handling here. CmdHandler can crash (its running
-- external code), so should be able to capture these:
-- https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
--
-- Also should eventually parallelize and other improvement on this region.
--
executeCmdRef :: [CmdHandler] -> BotEvent -> IO [Maybe BotCommand]
executeCmdRef cs m = mapM (\(CmdRef _ st h) -> h st m) cs
