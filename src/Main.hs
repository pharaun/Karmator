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
import Plugins.Karma.Types (Config)


testConfig :: ServerConfig
testConfig = ServerConfig "chat.freenode.net" 6697 ["levchius"] "Ghost Bot" Nothing True ["#gamelost"] "test.log"

-- TODO: clean up types, needs a better way to get ClockTime into uptime than this
--      - !karmatorjoin
--      - !karmatorleave
--      - !IRC_INVITE (invite command)
commandRoute :: Config -> ConnectionPool -> ClockTime -> Route [CmdHandler]
commandRoute c p t = choice
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
        handler "rawKarma" p (rawKarma c)

    , do
        match karmaSidevotesMatch
        debug "karmaSidevotesMatch"
        handler "karmaSideVotes" p (karmaSidevotes c)

    , do
        match karmaGiversMatch
        debug "karmaGiversMatch"
        handler "karmaGivers" p (karmaGivers c)

    , do
        match karmaMatch
        debug "karmaMatch"
        handler "karma" p (karma c)
    ]


main :: IO ()
main = runStderrLoggingT $ withSqlitePool "/tmp/test.db" 1 (\pool -> liftIO $ do
        -- Migrate the db
        flip runSqlPool pool $ (do
            runMigration migrateAll
            )

        -- Run the bot
        t <- getClockTime
        c <- getKarmaConfig "src/Plugins/Karma/parser.cfg"
        runBot [(True, testConfig)] (commandRoute c pool t)

        return ()
        )
