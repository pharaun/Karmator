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
main = establishTLS testConfig testPersistent
