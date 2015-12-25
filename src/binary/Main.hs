{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Data.ConfigFile
import Database.Persist.Sql hiding (get)
import Database.Persist.Sqlite hiding (get)
import System.Time
import Options.Applicative
import qualified Data.Text as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16

import qualified Network.Simple.TCP.TLS as TLS
import qualified Network.TLS as TLS
import qualified Data.X509.Validation as TLS

-- TODO: detect osx vs unix and get the cert store that way
--import qualified System.X509.Unix as TLS
import qualified System.X509.MacOS as TLS

-- Karmator
import Karmator.Bot
import Karmator.Route
import Karmator.Types

-- Plugins
import Plugins.Generic
import Plugins.Channels
import Plugins.Karma
import Karmator.State
import Plugins.Karma.Types (Config)

--
-- Routes configuration
--
commandRoute :: Config -> ConnectionPool -> ClockTime -> [(String, [BS.ByteString], [BS.ByteString], Int)] -> Route [CmdHandler]
commandRoute c p t nc = choice (
    [ do
        -- TODO: fix up pure needing a return here
        match pingMatch
        debug "pingMatch"
        pureHandler "ping" (return . ping)

    , do
        match uptimeMatch
        debug "uptimeMatch"
        stateHandler "uptime" t uptime

    -- Channel handlers
    -- TODO: do a pre-load command to pre-init/add the forced channel to list of channel to join or something
    -- TODO: maybe one possible thing is to offload all of the ConnectionPool into the bot config (since it'll be in the core)
    , do
        match inviteMatch
        debug "inviteMatch"
        persistHandler "invite" p (sqlWrapper inviteJoin)

    , do
        match joinMatch
        debug "joinMatch"
        persistHandler "join" p (sqlWrapper joinJoin)

    , do
        match partMatch
        debug "partMatch"
        persistHandler "part" p (sqlWrapper partLeave)

    , do
        match kickMatch
        debug "kickMatch"
        persistHandler "kick" p (sqlWrapper kickLeave)

    , do
        match listMatch
        debug "listMatch"
        persistHandler "list" p (sqlWrapper listChannel)

    -- Karma handlers
    -- Need to "create a database connection" then pass it into all karma handlers
    -- TODO: implement some sort of table escape hatch before switching to persist
    , do
        match rawKarmaMatch
        debug "rawKarmaMatch"
        persistHandler "rawKarma" p (sqlWrapper (rawKarma c))

    , do
        match karmaSidevotesMatch
        debug "karmaSidevotesMatch"
        persistHandler "karmaSideVotes" p (sqlWrapper (karmaSidevotes c))

    , do
        match karmaGiversMatch
        debug "karmaGiversMatch"
        persistHandler "karmaGivers" p (sqlWrapper (karmaGivers c))

    , do
        match karmaRankMatch
        debug "karmaRankMatch"
        persistHandler "karmaRank" p (sqlWrapper (karmaRank c))

    , do
        match karmaSidevotesRankMatch
        debug "karmaSidevotesRankMatch"
        persistHandler "karmaSidevotesRank" p (sqlWrapper (karmaSidevotesRank c))

    , do
        match karmaMatch
        debug "karmaMatch"
        persistHandler "karma" p (sqlWrapper (karma c))

    -- Per network MOTD join
    -- TODO: migrate channel routes here
    ] ++ map (\(n, cs, csBl, csJoin) -> do
        match (motdMatch n)
        debug ("motdMatch - " ++ n)
        persistHandler ("motd - " ++ n) p (sqlWrapper (motdJoin n cs))
        ) nc)


main :: IO ()
main = do
    botConf <- getArgs
    (database, karmaConf, servers, networkChannels) <- getBotConfig botConf
    runStderrLoggingT $ withSqlitePool database 1 (\pool -> liftIO $ do
        -- Run the bot
        t <- getClockTime
        c <- getKarmaConfig karmaConf

        -- Do the migration bit for simple state bits (don't touch karma bits yet)
        runSqlPool (runMigration migrateSimpleState) pool

        -- Run the bot
        runBot servers (commandRoute c pool t networkChannels)

        return ()
        )

-- CLI options
getArgs :: IO FilePath
getArgs = execParser opts
  where
    opts = info (helper <*> config)
        (  fullDesc
        <> progDesc "Run the 'Karmator' irc bot."
        <> header "karmator - An ircbot for handling karma" )
    config = strArgument
        (  metavar "CONFIG"
        <> help "The bot configuration"
        <> value "src/binary/bot.cfg"
        )

-- Load the bot config
getBotConfig :: FilePath -> IO (T.Text, FilePath, [ServerConfig], [(String, [BS.ByteString], [BS.ByteString], Int)])
getBotConfig conf = do
    config <- runExceptT (do
        c <- join $ liftIO $ readfile emptyCP conf

        -- Bot config
        -- TODO: look for a neat way to integrate handler/plugin config loaders here
        database  <- get c "bot" "database"
        karmaConf <- get c "bot" "karma_config"

        -- Get a list of section, each is a server config
        (servers, networkChannels) <- liftM splitConf $ mapM (getServerConfig c) $ filter ((/=) "bot") $ sections c

        return (database, karmaConf, servers, networkChannels))

    case config of
        Left cperr   -> error $ show cperr
        Right config -> return config
  where
    splitConf xs = (map fst xs, map snd xs)
    getServerConfig c s = do
        host      <- get c s "host"
        port      <- get c s "port"
        nicks     <- get c s "nicks"
        user      <- get c s "user"
        pass      <- get c s "pass"
        channel   <- get c s "channel" :: ExceptT CPError IO [BS.ByteString] -- Mandatory channels per host
        chan_bl   <- get c s "channel_blacklist" :: ExceptT CPError IO [BS.ByteString] -- Channel blacklist per host
        chan_join <- get c s "channel_joins"
        tlsHost   <- get c s "tls_host"
        tlsHash   <- get c s "tls_fingerprint" -- Hex sha256
        logfile   <- get c s "logfile"
        logirc    <- get c s "logirc"
        reconn    <- get c s "reconn"
        reWait    <- get c s "reconn_wait" -- In seconds

        tls <- case tlsHost of
            Nothing -> return Nothing
            Just th -> do
                -- Setup the TLS configuration
                tls <- liftIO $ TLS.makeClientSettings Nothing host (show port) True <$> TLS.getSystemCertificateStore
                let tls' = tls
                        { TLS.clientServerIdentification = (th, C8.pack $ show port)
                        , TLS.clientHooks = (TLS.clientHooks tls)
                            { TLS.onCertificateRequest = \_ -> return Nothing
                            }
                        }

                case tlsHash of
                    Nothing    -> return $ Just tls'
                    Just thash -> do
                        -- Setup hash
                        let unpackedHash = fst $ B16.decode $ C8.pack thash
                        let sid = (th, C8.pack $ show port)
                        let cache = TLS.exceptionValidationCache [(sid, TLS.Fingerprint unpackedHash)]

                        return $ Just $ tls'
                            { TLS.clientShared = (TLS.clientShared tls')
                                { TLS.sharedValidationCache = cache
                                }
                            }

        return (ServerConfig s host (fromInteger port) nicks user pass tls reconn (reWait * 1000000) logfile logirc, (s, channel, chan_bl, chan_join))
