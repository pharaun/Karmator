{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.List as DL

import qualified Data.ByteString as BS

import Control.Concurrent.STM.TVar (TVar)

-- Karmator
import Karmator.Bot
import Karmator.Route
import Karmator.Types
import Plugins.Filter
import Karmator.State

-- Plugins
import Plugins.Ping
import Plugins.Channels
import Plugins.Generic
import Plugins.Karma
import Plugins.Karma.Types (Config(..))

import qualified Network.IRC as IRC
import qualified Karmator.Server.IRC as IRC

import qualified Slack.Message as Slack
import qualified Karmator.Server.Slack as Slack

--
-- Config bits
--
import Data.ConfigFile
import Data.Set (Set)
import qualified Data.Set as Set
-- TODO: detect osx vs unix and get the cert store that way
import qualified System.X509.Unix as TLS
--import qualified System.X509.MacOS as TLS
import qualified Data.X509.Validation as TLS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import qualified Network.TLS as TLS
import qualified Pipes.Network.TCP.TLS as TLS
import qualified Network.Simple.TCP.TLS as TLS


--
-- Routes configuration
--
-- TODO: maybe one possible thing is to offload all of the ConnectionPool into the bot config (since it'll be in the core)
--
commandRoute :: Config -> ConnectionPool -> ClockTime -> (TVar PingDelay) -> [(String, [BS.ByteString], Set BS.ByteString, Int, [BS.ByteString])] -> Route [CmdHandler Slack.Message] Slack.Message
commandRoute c p t pd nc = choice (
    [ do
        match uptimeMatch
        debug "uptimeMatch"
        stateHandler "uptime" t uptime

    , do
        -- TODO: fix up pure needing a return here
        match versionMatch
        debug "versionMatch"
        pureHandler "version" (return . version)

    , do
        -- Bot Help text
        match $ customCommandMatch "!help"
        debug "customCommand - !help"
        pureHandler "!help" (return . (customCommand "Available commands: !uptime !version !github !sidevotes !karma !givers !rank !ranksidevote"))

    , do
        -- Bot Help text
        match $ customCommandMatch "!github"
        debug "customCommand - !github"
        pureHandler "!help" (return . (customCommand "Github repo: https://github.com/pharaun/Karmator"))

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

-- IRC pings
--    , do
--        match pingMatch
--        debug "pingMatch"
--        stateHandler "ping" pd ping
    ])
--    -- Per network channel supporting bits
--    ] ++ map (\(n, cs, csBl, csJoin, nicks) -> do
--        match (networkMatch n)
--        choice
--            [ do
--                match motdMatch
--                debug ("motdMatch - " ++ n)
--                persistHandler ("motd - " ++ n) p (sqlWrapper $ motdJoin n cs csBl csJoin)
--
--            -- Channel handlers
--            , do
--                match inviteMatch
--                debug ("inviteMatch - " ++ n)
--                persistHandler "invite" p (sqlWrapper $ inviteJoin n csBl)
--
--            , do
--                match joinMatch
--                debug ("joinMatch - " ++ n)
--                persistHandler "join" p (sqlWrapper $ joinJoin n csBl csJoin)
--
--            , do
--                match partMatch
--                debug ("partMatch - " ++ n)
--                persistHandler "part" p (sqlWrapper $ kickPartLeave n)
--
--            , do
--                match (kickMatch $ head nicks)
--                debug ("kickMatch - " ++ n)
--                persistHandler "kick" p (sqlWrapper $ kickPartLeave n)
--
--            , do
--                match listMatch
--                debug ("listMatch - " ++ n)
--                persistHandler "list" p (sqlWrapper $ listChannel n)
--            ]
--        ) nc)


main :: IO ()
main = do
    (botConf, version) <- getArgs

    if version
    then putStrLn versionText
    else do
        (database, karmaConf, ircServers, networkChannels, slackServers) <- getBotConfig botConf
        runStderrLoggingT $ withSqlitePool database 1 (\pool -> liftIO $ do
            -- Run the bot
            t <- getClockTime
            pd <- pingInit

            -- Run the bot
            --let serverRunners = map IRC.runServer ircServers ++ map Slack.runServer slackServers
            -- TODO: make this emit 2 runners (one for the irc network type, and 1 for slack network type)
            let serverRunners = map Slack.runServer slackServers
            runBot serverRunners (commandRoute karmaConf pool t pd networkChannels)

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
        <> value "karmator-bin/config/bot.cfg"
        )
        <*> switch
        ( long "version"
        <> help "The bot version"
        )


-- Load the bot config
-- TODO: do something like:
--  * Return a list [(type, [network tag])] for ie irc vs slack, and the
--      tags defined for each
--  * Then each server (knows its type, and its network tag) it can now
--      grab the server config
--  * Then (ie bot, plugin, etc) can poke the config to grab their block
--      getBlock(system, subsystem) ie (plugin, karma) -> gets its block
--      then it parses that block into the settings it care for
--
-- TODO: break out the ServerConfig parser, might be easier to just move
-- all parser here for now, but anyway we have.
-- ServerConfig, Bot Core, Bot Karma, network Irc, network slack parsers
--
getBotConfig :: FilePath -> IO (T.Text, Config, [ServerConfig IRC.IrcConfig], [(String, [BS.ByteString], Set BS.ByteString, Int, [BS.ByteString])], [ServerConfig Slack.SlackConfig])
getBotConfig conf = do
    config <- runExceptT (do
        c <- join $ liftIO $ readfile emptyCP conf

        -- Bot config
        -- TODO: look for a neat way to integrate handler/plugin config loaders here
        -- Maybe "[bot.database] database - settings"
        --       "[bot.karma] karma - settings"
        --       "[irc.networktag] irc server - settings"
        --       "[slack.networktag] slack server - settings"
        --
        -- Then stuff can register/add any config they need/want under
        -- these namespaces
        database  <- get c "bot" "database"
        karmaConf <- getKarmaConfig c

        -- Get a list of section, each is a server config
        (ircServers, networkChannels) <- liftM splitConf
                                       $ mapM (\s -> (do
                                                (z, l) <- getIrcServerConfig c s
                                                y <- getCommonServerConfig c s z
                                                return (y, l)))
                                       $ filter ("bot" /=)
                                       $ filter (DL.isPrefixOf "irc.")
                                       $ sections c

        slackServers <- mapM (\s -> getSlackServerConfig c s >>= getCommonServerConfig c s)
                      $ filter ("bot" /=)
                      $ filter (DL.isPrefixOf "slack.")
                      $ sections c

        return (database, karmaConf, ircServers, networkChannels, slackServers))

    case config of
        Left cperr   -> error $ show cperr
        Right config -> return config
  where
    splitConf xs = (map fst xs, map snd xs)


--
-- Load the karma config
--
getKarmaConfig :: ConfigParser -> ExceptT CPError IO Config
getKarmaConfig c = do
    strictMatch <- get c "bot.karma" "nick.strict_match"
    prefixMatch <- get c "bot.karma" "nick.prefix_match"
    suffixMatch <- get c "bot.karma" "nick.suffix_match"

    -- Force the Parser to invoke Read on the Partial/KarmaTypes
    partialKarma <- get c "bot.karma" "karma.partial"
    totalKarma   <- get c "bot.karma" "karma.total"

    return $ Config strictMatch prefixMatch suffixMatch partialKarma totalKarma


--
-- Server Config
--
getCommonServerConfig
    :: ConfigParser
    -> String
    -> a
    -> ExceptT CPError IO (ServerConfig a)
getCommonServerConfig c s nc = do
    logfile   <- get c s "common.logfile"
    lognet    <- get c s "common.lognet"
    reconn    <- get c s "common.reconn"
    reWait    <- get c s "common.reconn_wait" -- In seconds

    let network = DL.dropWhile ('.' ==) s
    -- TODO: make this take a func, and invoke the config for the network
    -- config, but need to standardize the irc config first
    return $ ServerConfig nc network reconn (reWait * 1000000) logfile lognet


--
-- Irc Server Config
--
getIrcServerConfig
    :: ConfigParser
    -> String
    -> ExceptT CPError IO (IRC.IrcConfig, (String, [BS.ByteString], Set BS.ByteString, Int, [BS.ByteString]))
getIrcServerConfig c s = do
    host      <- get c s "host"
    port      <- get c s "port"
    nicks     <- get c s "nicks"
    user      <- get c s "user"
    pass      <- get c s "pass"
    channel   <- get c s "channel" :: ExceptT CPError IO [BS.ByteString] -- Mandatory channels per host
    chan_bl   <- get c s "channel_blacklist" :: ExceptT CPError IO [BS.ByteString] -- Channel blacklist per host
    chan_join <- get c s "channel_joins" :: ExceptT CPError IO Int
    tlsHost   <- get c s "tls_host"
    tlsHash   <- get c s "tls_fingerprint" -- Hex sha256

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

    let network = DL.dropWhile ('.' ==) s
    let config = IRC.IrcConfig host (fromInteger port) nicks user pass tls
    return (config, (network, channel, Set.fromList chan_bl, chan_join, nicks))

--
-- Config
--
getSlackServerConfig
    :: ConfigParser
    -> String
    -> ExceptT CPError IO Slack.SlackConfig
getSlackServerConfig c s = do
    apitoken  <- get c s "api_token"
    return $ Slack.SlackConfig apitoken
