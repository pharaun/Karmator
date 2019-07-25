{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Plugins.Ping
    ( pingMatch
    , ping
    , pingInit

    , PingDelay
    ) where

import Karmator.Types
import Plugins.Filter
import qualified Network.IRC as IRC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.STM

-- Import the instance
import qualified Karmator.Server.IRC as IRC


--
-- Tunables
--
retries = 3 :: Int
timeout = 5 * 60 * 1000000 :: Int


-- TODO: find out the best way to monitor the pings, and if haven't gotten
-- one within X minute, ping back and after a few trys, if no response, go
-- ahead and trigger a reconnect
data PingDelay = PingDelay
    { tries :: Int
    , timerDead :: TMVar ()
    }


pingMatch :: BotEvent IRC.Message -> Bool
pingMatch = exactCommand $ C8.pack "PING"


--
-- Ping logic
-- 1. launch a thread to wait for timer to expire
--      a. if timer expires, bump tries +1, send a ping, and reset timer
--      b. after 3 retries send a disconnect signal somehow
--
-- 2. ping listener
--      a. if gets a pong or a ping reset retries and reset timer


-- TODO: Unsafe head
--ping :: Monad m => (TVar PingDelay) -> BotEvent -> m [BotCommand]
ping _ (EMessage m) = return [CMessage $ IRC.pong $ head $ IRC.msg_params m]
ping _ _ = return []


-- TODO: Add a way to give it a queue to inject something in?
pingInit :: IO (TVar PingDelay)
pingInit = atomically $ do
    dead <- newEmptyTMVar
    foo <- newTVar $ PingDelay 0 dead

    return foo

--uptimeMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!uptime")
--uptime t m  = do
--    now <- liftIO getClockTime
--    return [CMessage $ IRC.privmsg (whichChannel m) (C8.pack $ pretty $ diffClockTimes now t)]
