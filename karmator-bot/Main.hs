{-# LANGUAGE OverloadedStrings #-}

import System.Time
import Control.Monad.IO.Class
import Database.Persist.Sql hiding (get)
import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Applicative
import Network

import qualified Data.ByteString.Char8 as C8
import qualified Network.Simple.TCP.TLS as TLS
import qualified System.X509.Unix as TLS
import qualified Network.TLS as TLS

import Karmator.Bot
import Karmator.Route
import Karmator.Types

-- Plugins
import Plugins.Generic
import Plugins.Karma
import Plugins.Karma.Database
import Plugins.Karma.Types (Config)


--
-- Server configuration
--
freenodeConfig :: IO ServerConfig
freenodeConfig = do
    let host    = "weber.freenode.net"
    let tlsHost = "chat.freenode.net"
    let port    = 6697
    let channel = ["#gamelost"]
    let nicks   = ["levchius"]
    let user    = "Ghost Bot"
    let pass    = Nothing
    let reconn  = True
    let logfile = "test.log"

    -- Setup the TLS configuration
    tls <- TLS.makeClientSettings Nothing host (show port) True <$> TLS.getSystemCertificateStore
    let tls' = tls
            { TLS.clientServerIdentification = (tlsHost, C8.pack $ show port)
            , TLS.clientHooks = (TLS.clientHooks tls)
                { TLS.onCertificateRequest = \_ -> return Nothing
                }
            }

    return $ ServerConfig host port nicks user pass (Just tls') reconn channel logfile

--
-- Routes configuration
--
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
        -- Run the bot
        t <- getClockTime
        c <- getKarmaConfig "src/Plugins/Karma/parser.cfg"
        n <- freenodeConfig

        runBot [n] (commandRoute c pool t)

        return ()
        )
