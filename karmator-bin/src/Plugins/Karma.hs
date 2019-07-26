{-# LANGUAGE OverloadedStrings #-}
module Plugins.Karma
    ( karmaSidevotesMatch
    , karmaSidevotes

    -- Recievers
    , karmaMatch
    , karma

    -- Givers
    , karmaGiversMatch
    , karmaGivers

    -- Ranking
    , karmaRankMatch
    , karmaRank

    , karmaSidevotesRankMatch
    , karmaSidevotesRank

    , rawKarmaMatch
    , rawKarma

    , getKarmaConfig
    ) where

import Data.Maybe (fromMaybe)
import Control.Applicative
import Control.Monad.Except
import Data.Int
import Data.List
import Data.Time.Clock
import Database.Persist.Sql hiding (get)
import Control.Monad.Reader
import Formatting
import Text.Parsec
import qualified Data.ByteString as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- Karma module
import Plugins.Karma.Karma
import Plugins.Karma.Types
import Plugins.Karma.Database

-- Karmator
import Karmator.Types
import Plugins.Filter
import qualified Network.IRC as IRC
import qualified Network.IRC.Patch as IRC

-- Configuration
import Data.ConfigFile
import Prelude hiding (readFile)

-- Import the instance
import qualified Karmator.Server.IRC as IRC
import qualified Slack.Message as Slack
import Plugins.Generic (customCommand, customCommandMatch)

--
-- Karma Query
--
karmaMatch = customCommandMatch "!karma"

karma :: MonadIO m => Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
--karma = karmaStats KarmaReceived 3 False False
karma = karmaStats (KST KarmaReceived) 3

karmaGiversMatch = customCommandMatch "!givers"

karmaGivers :: MonadIO m => Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
--karmaGivers = karmaStats KarmaGiven 3 True False
karmaGivers = karmaStats (KST KarmaGiven) 3

karmaSidevotesMatch = customCommandMatch "!sidevotes"

karmaSidevotes :: MonadIO m => Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
--karmaSidevotes = karmaStats KarmaGiven 3 True True
karmaSidevotes = karmaStats KSTSidevote 3


data KarmaStatsType = KST KarmaTable | KSTSidevote

karmaStats :: (MonadIO m) => KarmaStatsType -> Int64 -> Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
karmaStats karmaType countMax conf m@(EMessage m') =
    --case parse karmaCommandParse "(irc)" $ T.decodeUtf8 $ messageContent m of
    case parse karmaCommandParse "(irc)" $ Slack.msg_text m' of
        (Left _)   -> return (customCommand "Karma command parse failed" m)
        (Right []) -> do
            (topNDesc, topNAsc) <- ask >>= (liftIO . runSqlPool
                (case karmaType of
                    (KST x) -> do
                        a <- topNDenormalized x countMax desc
                        b <- topNDenormalized x countMax asc
                        return (a, b)
                    KSTSidevote -> do
                        a <- topNSideVotesDenormalized KarmaReceived countMax
                        b <- topNSideVotesDenormalized KarmaGiven countMax
                        return (a, b)))

            return (customCommand (
                if null topNDesc || null topNAsc
                then "There is no karma values recorded in the database!"
                else TL.toStrict (
                    format (case karmaType of
                                (KST KarmaGiven)    -> "most positive: " % text % ". most negative: " % text % "."
                                (KST KarmaReceived) -> "highest karma: " % text % ". lowest karma: " % text % "."
                                KSTSidevote         -> "most sidevotes received: " % text % ". most sidevotes given: " % text % ".")
                           (renderTotalKarma topNDesc)
                           (renderTotalKarma topNAsc)
                    )
                ) m)
        (Right x)  ->
            case karmaType of
                (KST x') -> do
                    count <- (liftIO . runSqlPool (partalKarma x' x)) =<< ask
                    return (customCommand (TL.toStrict $ renderAllKarma count) m)
                KSTSidevote -> return (customCommand "Not supported!" m)
karmaStats _ _ _ _ = return []


renderAllKarma :: [(T.Text, Int, Int, Int)] -> TL.Text
renderAllKarma xs = TL.intercalate "; " $ map (\(name, p, n, s) -> format (stext % ", " % int % " (" % int % "++/" % int % "--/" % int % "+-)") name (p-n) p n s) xs

renderTotalKarma :: [(T.Text, Int)] -> TL.Text
renderTotalKarma xs = TL.intercalate "; " $ map (uncurry (format (stext % " (" % int % ")"))) xs


--
-- Rank commands
--
karmaRankMatch = customCommandMatch "!rank"

karmaRank :: MonadIO m => Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
karmaRank = handleKarmaRank False

karmaSidevotesRankMatch = customCommandMatch "!ranksidevote"

karmaSidevotesRank :: MonadIO m => Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
karmaSidevotesRank = handleKarmaRank True


handleKarmaRank :: MonadIO m => Bool -> Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
handleKarmaRank sidevotes conf m@(EMessage m') =
    --case parse karmaCommandParse "(irc)" $ T.decodeUtf8 $ messageContent m of
    case parse karmaCommandParse "(irc)" $ Slack.msg_text m' of
        (Left _)       -> return (customCommand "Karma command parse failed" m)
        (Right x)      -> do
            --let nick  = T.decodeUtf8 $ nickContent m
            let nick = fromMaybe "" $ Slack.msg_user m'
            msg <- case x of
                []   -> renderRank sidevotes nick nick "Your"
                [x'] -> renderRank sidevotes nick x' x'
                _    -> return "Can only rank one karma entry at a time!"

            return (customCommand (TL.toStrict msg) m)
handleKarmaRank _ _ _ = return []

renderRank :: (MonadIO m) => Bool -> T.Text -> T.Text -> T.Text -> ReaderT ConnectionPool m TL.Text
renderRank sidevotes nick whom target = do
    (recvRank, recvCount, giveRank, giveCount) <- ask >>= (liftIO . runSqlPool (do
        let call = if sidevotes
                   then sideVotesRankingDenormalized
                   else rankingDenormalized

        recvRank <- call KarmaReceived whom
        recvCount <- countK KarmaReceived whom
        giveRank <- call KarmaGiven whom
        giveCount <- countK KarmaGiven whom

        return (recvRank, recvCount, giveRank, giveCount)
        ))

    return $ case (recvRank, giveRank) of
            (Nothing, Nothing) -> "No ranking available"
            (Just r,  Nothing) -> renderLine target r recvCount "receiving."
            (Nothing, Just g)  -> renderLine target g giveCount "giving."
            (Just r,  Just g)  -> format (text % " and " % text)
                                         (renderLine target r recvCount "receiving")
                                         (renderLine target g giveCount "giving.")
  where
    renderLine = format (stext % " rank is " % int % " of " % int % " in " % text)

--
-- Karma Ingestion
--
rawKarmaMatch :: BotEvent Slack.Message -> Bool
rawKarmaMatch = liftM2 (&&) (exactCommand $ C8.pack "PRIVMSG") (not . prefixMessage (C8.pack "!"))

rawKarma :: MonadIO m => Config -> BotEvent Slack.Message -> ReaderT ConnectionPool m [BotCommand Slack.Message]
rawKarma conf m@(EMessage m') = do
    -- ByteString -> utf8
    -- TODO: error handling
    --let msg   = T.decodeUtf8 $ messageContent m
    let msg = Slack.msg_text m'
    --let nick  = T.decodeUtf8 $ nickContent m
    let nick = fromMaybe "" $ Slack.msg_user m'

    case parseInput conf nick msg of
        (KarmaReply n (Just k)) -> do
            -- TODO: not for certain if we want to preserve '~' in username or not
            -- hikmet|hikmet|UKZC7K33J|SlackServer
            -- cleaned_nick | full_name | username | hostmask
            --let user  = T.decodeUtf8 <$> userNameContent m
            let user = Just $ Slack.msg_uid m'
            --let host  = T.decodeUtf8 <$> hostMaskContent m
            let host = Just "SlackServer"

            -- TODO: identify how this handles privmsg
            -- TODO: identify how it handles multiple channel (Do we want list here) also is privmsg specified or Null?
            --  * It lists the bot's nick as a channel, which works for me.  !karma/etc are busted as a privmsg tho
            --  * the irc handler can handle this (by taking 1 msg -> generating several)
            --let chan  = Just $ T.decodeUtf8 $ whichChannel m
            --let chan = Slack.msg_channel m'
            let chan = Just $ Slack.msg_cid m'

            t <- liftIO getCurrentTime
            liftIO . runSqlPool (addKarma t n nick user host chan k) =<< ask
            return []
        _                       -> return []
rawKarma _ _ = return []

parseInput :: Config -> T.Text -> T.Text -> KarmaReply
parseInput config jnick jinput =
    -- Raw input, no json needed
    if filterBot config jnick
    then KarmaReply jnick Nothing
        -- Parse the message
    else case (parse (karmaParse config) "(stdin)" jinput) of
        (Left _)  -> KarmaReply (cleanNick jnick) Nothing -- Failed to find a karma entry

        (Right k) ->
            -- Scan the extracted karma expression for the nick
            if null (filterKarma [cleanNick jnick, jnick] k)
            then KarmaReply (cleanNick jnick) Nothing
            else KarmaReply (cleanNick jnick) (Just $ filterKarma [cleanNick jnick, jnick] k)
  where
    -- Cleanup the nickname
    cleanNick nick = case parse nickDeFuzzifier "(stdin)" nick of
        (Left _)  -> nick
        (Right n) -> n

    filterKarma :: [T.Text] -> [Karma] -> [Karma]
    filterKarma n = filter (\Karma{kMessage=msg} -> msg `DL.notElem` n)


--
-- Load the config
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
