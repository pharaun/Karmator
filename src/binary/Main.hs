{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Data.ConfigFile
import Database.Persist.Sql hiding (get)
import Database.Persist.Sqlite hiding (get)
import System.Time
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as C8

import qualified Network.Simple.TCP.TLS as TLS
import qualified Network.TLS as TLS
import qualified System.X509.Unix as TLS

-- Karmator
import Karmator.Bot
import Karmator.Route
import Karmator.Types

-- Plugins
import Plugins.Generic
import Plugins.Karma
import Plugins.Karma.Types (Config)

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

    -- TODO: this seems non-functional, should roll it into the autojoin
    -- logic
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
main = do
    -- Externalize/argv the botconfig filepath
    (database, karmaConf, servers) <- getBotConfig "src/binary/bot.cfg"
    runStderrLoggingT $ withSqlitePool database 1 (\pool -> liftIO $ do
        -- Run the bot
        t <- getClockTime
        c <- getKarmaConfig karmaConf

        runBot servers (commandRoute c pool t)

        return ()
        )

-- Load the bot config
getBotConfig :: FilePath -> IO (T.Text, FilePath, [ServerConfig])
getBotConfig conf = do
    config <- runExceptT (do
        c <- join $ liftIO $ readfile emptyCP conf

        -- Bot config
        database  <- get c "bot" "database"
        karmaConf <- get c "bot" "karma_config"

        -- Get a list of section, each is a server config
        servers <- mapM (getServerConfig c) $ filter ((/=) "bot") $ sections c

        return (database, karmaConf, servers))

    case config of
        Left cperr   -> error $ show cperr
        Right config -> return config
  where
    getServerConfig c s = do
        host    <- get c s "host"
        port    <- get c s "port"
        nicks   <- get c s "nicks"
        user    <- get c s "user"
        pass    <- get c s "pass"
        channel <- get c s "channel"
        tlsHost <- get c s "tls_host"
        logfile <- get c s "logfile"
        logirc  <- get c s "logirc"
        reconn  <- get c s "reconn"
        reWait  <- get c s "reconn_wait" -- In seconds

        tls <- case tlsHost of
            Nothing -> return Nothing
            Just th -> do
                -- Setup the TLS configuration
                tls <- liftIO $ TLS.makeClientSettings Nothing host (show port) True <$> TLS.getSystemCertificateStore
                return $ Just $ tls
                        { TLS.clientServerIdentification = (th, C8.pack $ show port)
                        , TLS.clientHooks = (TLS.clientHooks tls)
                            { TLS.onCertificateRequest = \_ -> return Nothing
                            }
                        }

        return $ ServerConfig host (fromInteger port) nicks user pass tls reconn (reWait * 1000000) channel logfile logirc
