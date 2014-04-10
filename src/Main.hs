{-# LANGUAGE OverloadedStrings #-}

import Karmator.Bot
import Karmator.Types
--import Plugins.Ping

--
-- TODO: Add in all of the plugin configuration
--


testConfig :: ServerConfig
testConfig = ServerConfig "chat.freenode.net" 6697 ["levchius"] "Ghost Bot" Nothing True "test.log"

testPersistent :: ServerPersistentState
testPersistent = ServerPersistentState ["#test"]

main :: IO ()
main = establishTLS testConfig testPersistent
