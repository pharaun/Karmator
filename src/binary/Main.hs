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
import Data.Set (Set)
import Data.Monoid ((<>))
import qualified Data.Set as Set

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16

import qualified Network.Simple.TCP.TLS as TLS
import qualified Network.TLS as TLS
import qualified Data.X509.Validation as TLS

-- TODO: detect osx vs unix and get the cert store that way
import qualified System.X509.Unix as TLS
--import qualified System.X509.MacOS as TLS

import Control.Concurrent.STM.TVar (TVar)

-- Karmator
import Karmator.Bot
import Karmator.Route
import Karmator.Types
import Karmator.Filter

-- Plugins
import Plugins.Ping
import Plugins.Generic
import Plugins.Channels
import Plugins.Karma
import Karmator.State
import Plugins.Karma.Types (Config)

--
-- Routes configuration
--
-- TODO: maybe one possible thing is to offload all of the ConnectionPool into the bot config (since it'll be in the core)
--
commandRoute :: Config -> ConnectionPool -> ClockTime -> (TVar PingDelay) -> [(String, [BS.ByteString], Set BS.ByteString, Int, [BS.ByteString])] -> Route [CmdHandler]
commandRoute c p t pd nc = choice (
    [ do
        match pingMatch
        debug "pingMatch"
        stateHandler "ping" pd ping

    , do
        match uptimeMatch
        debug "uptimeMatch"
        stateHandler "uptime" t uptime

    , do
        -- TODO: fix up pure needing a return here
        match versionMatch
        debug "versionMatch"
        pureHandler "version" (return . version)

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

    -- Per network channel supporting bits
    ] ++ map (\(n, cs, csBl, csJoin, nicks) -> do
        match (networkMatch n)
        choice
            [ do
                match motdMatch
                debug ("motdMatch - " ++ n)
                persistHandler ("motd - " ++ n) p (sqlWrapper $ motdJoin n cs csBl csJoin)

            -- Channel handlers
            , do
                match inviteMatch
                debug ("inviteMatch - " ++ n)
                persistHandler "invite" p (sqlWrapper $ inviteJoin n csBl)

            , do
                match joinMatch
                debug ("joinMatch - " ++ n)
                persistHandler "join" p (sqlWrapper $ joinJoin n csBl csJoin)

            , do
                match partMatch
                debug ("partMatch - " ++ n)
                persistHandler "part" p (sqlWrapper $ kickPartLeave n)

            , do
                match (kickMatch $ head nicks)
                debug ("kickMatch - " ++ n)
                persistHandler "kick" p (sqlWrapper $ kickPartLeave n)

            , do
                match listMatch
                debug ("listMatch - " ++ n)
                persistHandler "list" p (sqlWrapper $ listChannel n)
            ]
        ) nc)


main :: IO ()
main = do
    (botConf, version) <- getArgs

    if version
    then putStrLn versionText
    else do
        (database, karmaConf, servers, networkChannels) <- getBotConfig botConf
        runStderrLoggingT $ withSqlitePool database 1 (\pool -> liftIO $ do
            -- Run the bot
            t <- getClockTime
            c <- getKarmaConfig karmaConf
            pd <- pingInit

            -- Run the bot
            runBot servers (commandRoute c pool t pd networkChannels)

            return ()
            )

-- CLI options
getArgs :: IO (FilePath, Bool)
getArgs = execParser opts
  where
    opts = info (helper <*> config)
        (  fullDesc
        <> progDesc "Run the 'Karmator' irc bot."
        <> header "karmator - An ircbot for handling karma" )
    config = (,)
        <$> strArgument
        (  metavar "CONFIG"
        <> help "The bot configuration"
        <> value "src/binary/bot.cfg"
        )
        <*> switch
        ( long "version"
        <> help "The bot version"
        )

-- Load the bot config
getBotConfig :: FilePath -> IO (T.Text, FilePath, [ServerConfig], [(String, [BS.ByteString], Set BS.ByteString, Int, [BS.ByteString])])
getBotConfig conf = do
    config <- runExceptT (do
        c <- join $ liftIO $ readfile emptyCP conf

        -- Bot config
        -- TODO: look for a neat way to integrate handler/plugin config loaders here
        database  <- get c "bot" "database"
        karmaConf <- get c "bot" "karma_config"

        -- Get a list of section, each is a server config
        (servers, networkChannels) <- liftM splitConf $ mapM (getServerConfig c) $ filter ("bot" /=) $ sections c

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

        return (ServerConfig s host (fromInteger port) nicks user pass tls reconn (reWait * 1000000) logfile logirc, (s, channel, Set.fromList chan_bl, chan_join, nicks))
