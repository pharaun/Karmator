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
import qualified System.X509.Unix as TLS
import qualified Data.X509.Validation as TLS

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
commandRoute :: Config -> ConnectionPool -> ClockTime -> [(String, [BS.ByteString])] -> Route [CmdHandler]
commandRoute c p t nc = choice (
    [ do
        match pingMatch
        debug "pingMatch"
        handler "ping" () (\_ i -> return $ ping i)

    , do
        match uptimeMatch
        debug "uptimeMatch"
        handler "uptime" t uptime

    , do
        match inviteMatch
        debug "inviteMatch"
        handler "invite" () (\_ i -> return $ inviteJoin i)

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
        match karmaRankMatch
        debug "karmaRankMatch"
        handler "karmaRank" p (karmaRank c)

    , do
        match karmaSidevotesRankMatch
        debug "karmaSidevotesRankMatch"
        handler "karmaSidevotesRank" p (karmaSidevotesRank c)

    , do
        match karmaMatch
        debug "karmaMatch"
        handler "karma" p (karma c)

    -- Per network MOTD join
    ] ++ map (\(n, cs) -> do
        match (motdMatch n)
        debug ("motdMatch - " ++ n)
        handler ("motd - " ++ n) () (\_ i -> return $ motdJoin cs i)
        ) nc)


main :: IO ()
main = do
    botConf <- getArgs
    (database, karmaConf, servers, networkChannels) <- getBotConfig botConf
    runStderrLoggingT $ withSqlitePool database 1 (\pool -> liftIO $ do
        -- Run the bot
        t <- getClockTime
        c <- getKarmaConfig karmaConf

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
getBotConfig :: FilePath -> IO (T.Text, FilePath, [ServerConfig], [(String, [BS.ByteString])])
getBotConfig conf = do
    config <- runExceptT (do
        c <- join $ liftIO $ readfile emptyCP conf

        -- Bot config
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
        host    <- get c s "host"
        port    <- get c s "port"
        nicks   <- get c s "nicks"
        user    <- get c s "user"
        pass    <- get c s "pass"
        channel <- get c s "channel" :: ExceptT CPError IO [BS.ByteString]
        tlsHost <- get c s "tls_host"
        tlsHash <- get c s "tls_fingerprint" -- Hex sha256
        logfile <- get c s "logfile"
        logirc  <- get c s "logirc"
        reconn  <- get c s "reconn"
        reWait  <- get c s "reconn_wait" -- In seconds

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

        return (ServerConfig s host (fromInteger port) nicks user pass tls reconn (reWait * 1000000) logfile logirc, (s, channel))
