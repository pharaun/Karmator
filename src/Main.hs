{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import System.Time

import Karmator.Bot
import Karmator.Route
import Karmator.Types
import Plugins.Ping


testConfig :: ServerConfig
testConfig = ServerConfig "chat.freenode.net" 6697 ["levchius"] "Ghost Bot" Nothing True "test.log"

testPersistent :: ServerPersistentState
testPersistent = ServerPersistentState ["#test"]

testBotConfig :: BotConfig
testBotConfig = BotConfig

-- TODO: clean up types, needs a better way to get ClockTime into uptime than this
-- TODO: starting to dislike the consistant (Monad m, MonadIO m) let's see if we can't clean this type crap up too
commandRoute :: ClockTime -> Route [CmdHandler]
commandRoute t = choice
    [ do
        match pingMatch
        debug "pingMatch"
        handler "ping" () (\_ i -> return $ ping i)
    , do
        match motdMatch
        debug "motdMatch"
        handler "match" () (\_ i -> return $ motdJoin i)
    , do
        match uptimeMatch
        debug "uptimeMatch"
        handler "uptime" t uptime
    ]

-- TODO:
--  Ideal schema would be like:
--
--  withIRC botConfig \i -> do
--      addServer i serverConfig
--      addServer i serverConfig2
--
--      addHook i xyz
--      addRoute i xyz
--      ....
main :: IO ()
main =
    withIRC testBotConfig $ \i -> do
        t <- getClockTime
        i'  <- addServer i True testConfig testPersistent
        addRoute i' (commandRoute t)
