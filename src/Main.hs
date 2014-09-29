{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import System.Time

import Karmator.Bot
import Karmator.Route
import Karmator.Types
import Plugins.Ping


testConfig :: ServerConfig
testConfig = ServerConfig "chat.freenode.net" 6697 ["levchius"] "Ghost Bot" Nothing True ["#gamelost"] "test.log"

-- TODO: clean up types, needs a better way to get ClockTime into uptime than this
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

main :: IO ()
main = do
    t <- getClockTime
    runBot [(True, testConfig)] (commandRoute t)
