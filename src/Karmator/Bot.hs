{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    ( runBot
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


-- TODO: Nice to move TLS config into ServerConfig, Also perhaps nuke ServerPersisitentState
runBot :: [(Bool, ServerConfig)] -> Route [CmdHandler] -> IO ()
runBot s r = do
    -- Start up the bot command + route
    q <- newTQueueIO
    bot <- async (runCommand q [r])

    -- Give the input to each server thread and spawn them
    servers <- mapM (\(tls, sc) -> async (runServer tls sc q)) s
    waitAny (bot : servers)
    return ()

runCommand :: TQueue (IRC.Message, TQueue IRC.Message) -> [Route [CmdHandler]] -> IO ()
runCommand q routes = forever $ do
    (msg, reply) <- atomically $ readTQueue q

    cmdRefs <- runRoute (choice routes) msg
    results <- executeCmdRef cmdRefs msg

    mapM (atomically . writeTQueue reply) (catMaybes results)
