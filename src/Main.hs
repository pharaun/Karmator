{-# LANGUAGE OverloadedStrings #-}

import System.Time
import Control.Monad.IO.Class
import Database.Persist.Sql hiding (get)
import Database.Persist.Sqlite
import Control.Monad.Logger

import Karmator.Bot
import Karmator.Route
import Karmator.Types

-- Plugins
import Plugins.Generic
import Plugins.Karma
import Plugins.Karma.Database


testConfig :: ServerConfig
testConfig = ServerConfig "chat.freenode.net" 6697 ["levchius"] "Ghost Bot" Nothing True ["#gamelost"] "test.log"

-- TODO: clean up types, needs a better way to get ClockTime into uptime than this
commandRoute :: ConnectionPool -> ClockTime -> Route [CmdHandler]
commandRoute p t = choice
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

    -- Karma handlers
    -- Need to "create a database connection" then pass it into all karma handlers
    , do
        match rawKarmaMatch
        debug "rawKarmaMatch"
        handler "rawKarma" p rawKarma
    ]


main :: IO ()
main = runStderrLoggingT $ withSqlitePool ":memory:" 1 (\pool -> liftIO $ do
        -- Migrate the db
        flip runSqlPool pool $ (do
            runMigration migrateAll
            )

        -- Run the bot
        t <- getClockTime
        runBot [(True, testConfig)] (commandRoute pool t)

        return ()
        )
