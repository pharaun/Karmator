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
import Karmator.Filter
import qualified Network.IRC as IRC
import qualified Network.IRC.Patch as IRC

-- Configuration
import Data.ConfigFile
import Prelude hiding (readFile)


--
-- Karma Query
--
karmaMatch :: BotEvent IRC.Message -> Bool
karmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!karma")

karma :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karma = karmaStats KarmaReceived 3 False False

karmaGiversMatch :: BotEvent IRC.Message -> Bool
karmaGiversMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!givers")

karmaGivers :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaGivers = karmaStats KarmaGiven 3 True False

karmaSidevotesMatch :: BotEvent IRC.Message -> Bool
karmaSidevotesMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!sidevotes")

karmaSidevotes :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaSidevotes = karmaStats KarmaGiven 3 True True


-- TODO: tritype (give|get|side, rather than two boolean)
karmaStats :: (MonadIO m) => KarmaTable -> Int64 -> Bool -> Bool -> Config -> BotEvent IRC.Message -> m [BotCommand IRC.Message]
karmaStats karmaType countMax givers sidevote conf m@(EMessage _ _) =
    case parse karmaCommandParse "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)   -> return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Karma command parse failed"]
        (Right []) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask

            (topNDesc, topNAsc) <- liftIO $ flip runSqlPool pool $
                if sidevote
                then do
                    a <- topNSideVotesDenormalized KarmaReceived countMax
                    b <- topNSideVotesDenormalized KarmaGiven countMax
                    return (a, b)
                else do
                    a <- topNDenormalized karmaType countMax desc
                    b <- topNDenormalized karmaType countMax asc
                    return (a, b)

            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) (
                if null topNDesc || null topNAsc
                then "There is no karma values recorded in the database!"
                else BL.toStrict $ TL.encodeUtf8 (
                    format (if sidevote
                            then "most sidevotes received: " % text % ". most sidevotes given: " % text % ". "
                            else if givers
                                 then "most positive: " % text % ". most negative: " % text % "."
                                 else "highest karma: " % text % ". lowest karma: " % text % ".")
                           (renderTotalKarma topNDesc)
                           (renderTotalKarma topNAsc)
                    )
                )]
        (Right x)  -> do
            if sidevote
            then return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Not supported!"]
            else do
                let nick  = T.decodeUtf8 $ nickContent m
                pool <- ask

                count <- liftIO $ runSqlPool (partalKarma karmaType x) pool
                return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) $ BL.toStrict $ TL.encodeUtf8 (renderAllKarma count)]
karmaStats _ _ _ _ _ _ _ = return []

renderAllKarma :: [(T.Text, Int, Int, Int)] -> TL.Text
renderAllKarma xs = TL.intercalate "; " $ map (\(name, p, n, s) -> format (stext % ", " % int % " (" % int % "++/" % int % "--/" % int % "+-)") name (p-n) p n s) xs

renderTotalKarma :: [(T.Text, Int)] -> TL.Text
renderTotalKarma xs = TL.intercalate "; " $ map (uncurry (format (stext % " (" % int % ")"))) xs


--
-- Rank commands
--
karmaRankMatch :: BotEvent IRC.Message -> Bool
karmaRankMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!rank")

karmaRank :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaRank = handleKarmaRank False

karmaSidevotesRankMatch :: BotEvent IRC.Message -> Bool
karmaSidevotesRankMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!ranksidevote")

karmaSidevotesRank :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaSidevotesRank = handleKarmaRank True


handleKarmaRank :: MonadIO m => Bool -> Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
handleKarmaRank sidevotes conf m@(EMessage _ _) =
    case parse karmaCommandParse "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)       -> return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Karma command parse failed"]
        (Right x)      -> do
            let nick  = T.decodeUtf8 $ nickContent m
            msg <- case x of
                []   -> do
                    pool <- ask
                    renderRank pool sidevotes nick nick "Your"
                [x'] -> do
                    pool <- ask
                    renderRank pool sidevotes nick x' x'
                _    -> return "Can only rank one karma entry at a time!"

            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) (BL.toStrict $ TL.encodeUtf8 msg)]
handleKarmaRank _ _ _ = return []

renderRank :: (MonadIO m) => ConnectionPool -> Bool -> T.Text -> T.Text -> T.Text -> m TL.Text
renderRank pool sidevotes nick whom target = do
    (recvRank, recvCount, giveRank, giveCount) <- liftIO $ flip runSqlPool pool (do
        let call = if sidevotes
                   then sideVotesRankingDenormalized
                   else rankingDenormalized

        recvRank <- call KarmaReceived whom
        recvCount <- countK KarmaReceived whom
        giveRank <- call KarmaGiven whom
        giveCount <- countK KarmaGiven whom

        return (recvRank, recvCount, giveRank, giveCount)
        )

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
rawKarmaMatch :: BotEvent IRC.Message -> Bool
rawKarmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (not . prefixMessage "!")

rawKarma :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
rawKarma conf m@(EMessage _ _) = do
    -- ByteString -> utf8
    -- TODO: error handling
    let msg   = T.decodeUtf8 $ messageContent m
    let nick  = T.decodeUtf8 $ nickContent m

    case parseInput conf nick msg of
        (KarmaReply n (Just k)) -> do
            t <- liftIO getCurrentTime
            pool <- ask

            -- TODO: not for certain if we want to preserve '~' in username or not
            let user  = T.decodeUtf8 <$> userNameContent m
            let host  = T.decodeUtf8 <$> hostMaskContent m

            -- TODO: identify how this handles privmsg
            -- TODO: identify how it handles multiple channel (Do we want list here) also is privmsg specified or Null?
            --  * It lists the bot's nick as a channel, which works for me.  !karma/etc are busted as a privmsg tho
            --  * the irc handler can handle this (by taking 1 msg -> generating several)
            let chan  = Just $ T.decodeUtf8 $ whichChannel m

            liftIO $ runSqlPool (addKarma t n nick user host chan k) pool
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
getKarmaConfig :: FilePath -> IO Config
getKarmaConfig conf = do
    config <- runExceptT (do
        c <- join $ liftIO $ readfile emptyCP conf

        strictMatch <- get c "nick_filtering" "strict_match"
        prefixMatch <- get c "nick_filtering" "prefix_match"
        suffixMatch <- get c "nick_filtering" "suffix_match"

        -- Force the Parser to invoke Read on the Partial/KarmaTypes
        partialKarma <- get c "karma_parsing" "partial"
        totalKarma <- get c "karma_parsing" "total"

        return $ Config strictMatch prefixMatch suffixMatch partialKarma totalKarma)

    case config of
        Left cperr   -> error $ show cperr
        Right config -> return config
