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
-- Karma list of unit rendering
--
renderAllKarma :: [(T.Text, Int, Int, Int)] -> TL.Text
renderAllKarma xs = TL.intercalate "; " $ map (\(name, p, n, s) -> format (stext % ", " % int % " (" % int % "++/" % int % "--/" % int % "+-)") name (p-n) p n s) xs

renderTotalKarma :: [(T.Text, Int)] -> TL.Text
renderTotalKarma xs = TL.intercalate "; " $ map (uncurry (format (stext % " (" % int % ")"))) xs


karmaSidevotesMatch :: BotEvent IRC.Message -> Bool
karmaSidevotesMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!sidevotes")

karmaSidevotes :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaSidevotes _ m@(EMessage _ _) = do
    let nick     = T.decodeUtf8 $ nickContent m
    let countMax = 3

    pool <- ask

    (received, given) <- liftIO $ flip runSqlPool pool $ do
            a <- topNSideVotesDenormalized KarmaReceived countMax
            b <- topNSideVotesDenormalized KarmaGiven countMax
            return (a, b)

    return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) (
        if null received || null given
        then "There is no karma values recorded in the database!"
        else BL.toStrict $ TL.encodeUtf8 (
            format ("most sidevotes received: " % text % ". most sidevotes given: " % text % ". ")
                   (renderTotalKarma received)
                   (renderTotalKarma given)
            )
        )]
karmaSidevotes _ _ = return []


karmaMatch :: BotEvent IRC.Message -> Bool
karmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!karma")

karma :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karma conf m = do
    pool <- ask
    karmaStats conf pool KarmaReceived 3 False m


karmaGiversMatch :: BotEvent IRC.Message -> Bool
karmaGiversMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!givers")

karmaGivers :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaGivers conf m = do
    pool <- ask
    karmaStats conf pool KarmaGiven 3 True m


karmaStats :: (MonadIO m) => Config -> ConnectionPool -> KarmaTable -> Int64 -> Bool -> BotEvent IRC.Message -> m [BotCommand IRC.Message]
karmaStats conf pool karmaType countMax givers m@(EMessage _ _) =
    case parse (karmaCommandParse conf) "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)   -> return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Karma command parse failed"]
        (Right []) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            (topNDesc, topNAsc) <- liftIO $ flip runSqlPool pool $ do
                    a <- topNDenormalized karmaType countMax desc
                    b <- topNDenormalized karmaType countMax asc
                    return (a, b)

            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) (
                if null topNDesc || null topNAsc
                then "There is no karma values recorded in the database!"
                else BL.toStrict $ TL.encodeUtf8 (
                    format (if givers
                            then "most positive: " % text % ". most negative: " % text % "."
                            else "highest karma: " % text % ". lowest karma: " % text % ".")
                           (renderTotalKarma topNDesc)
                           (renderTotalKarma topNAsc)
                    )
                )]
        (Right x)  -> do
            let nick  = T.decodeUtf8 $ nickContent m
            count <- liftIO $ runSqlPool (partalKarma karmaType x) pool
            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) $ BL.toStrict $ TL.encodeUtf8 (renderAllKarma count)]
karmaStats _ _ _ _ _ _ = return []


karmaRankMatch :: BotEvent IRC.Message -> Bool
karmaRankMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!rank")


karmaRank :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaRank conf m@(EMessage _ _) =
    case parse (karmaCommandParse conf) "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)       -> return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Karma command parse failed"]
        (Right [])     -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool False nick nick "Your"
            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) msg]
        (Right [x]) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool False nick x x
            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) msg]
        (Right _)      -> return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Can only rank one karma entry at a time!"]
karmaRank _ _ = return []


-- TODO: probably should make this remain T.Text and move encoding else where
renderRank :: (MonadIO m) => ConnectionPool -> Bool -> T.Text -> T.Text -> T.Text -> m B.ByteString
renderRank pool sidevotes nick whom target = do
    (recvRank, recvCount, giveRank, giveCount) <- liftIO $ flip runSqlPool pool $
        if sidevotes
        then (do
            a <- sideVotesRankingDenormalized KarmaReceived whom
            b <- countK KarmaReceived whom
            c <- sideVotesRankingDenormalized KarmaGiven whom
            d <- countK KarmaGiven whom
            return (a, b, c, d)
            )
        else (do
            a <- rankingDenormalized KarmaReceived whom
            b <- countK KarmaReceived whom
            c <- rankingDenormalized KarmaGiven whom
            d <- countK KarmaGiven whom
            return (a, b, c, d)
            )

    -- TODO: clean this up
    return $ BL.toStrict $ TL.encodeUtf8 (
        case (recvRank, giveRank) of
            (Nothing, Nothing) -> "No ranking available"
            (Just r,  Nothing) -> format (stext % " rank is " % int % " of " % int % " in receiving.") target r recvCount
            (Nothing, Just g)  -> format (stext % " rank is " % int % " of " % int % " in giving.") target g giveCount
            (Just r,  Just g)  -> format (stext % " rank is " % int % " of " % int % " in receiving and " % int % " of " % int % " in giving.") target r recvCount g giveCount
        )


karmaSidevotesRankMatch :: BotEvent IRC.Message -> Bool
karmaSidevotesRankMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!ranksidevote")

karmaSidevotesRank :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
karmaSidevotesRank conf m@(EMessage _ _) =
    case parse (karmaCommandParse conf) "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)       -> return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Karma command parse failed"]
        (Right [])     -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool True nick nick "Your"
            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) msg]
        (Right [x]) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool True nick x x
            return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) msg]
        (Right _)      -> return [CMessage $ IRC.privmsgnick (whichChannel m) (nickContent m) "Can only rank one karma entry at a time!"]
karmaSidevotesRank _ _ = return []


-- This takes care of sulping all irc messages to stuff into the karma parser
rawKarmaMatch :: BotEvent IRC.Message -> Bool
rawKarmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (not . prefixMessage "!")

rawKarma :: MonadIO m => Config -> BotEvent IRC.Message -> ReaderT ConnectionPool m [BotCommand IRC.Message]
rawKarma conf m@(EMessage _ _) = do
    -- ByteString -> utf8
    -- TODO: error handling
    let msg   = T.decodeUtf8 $ messageContent m
    let nick  = T.decodeUtf8 $ nickContent m
    -- TODO: not for certain if we want to preserve '~' in username or not
    let user  = T.decodeUtf8 <$> userNameContent m
    let host  = T.decodeUtf8 <$> hostMaskContent m
    -- TODO: identify how this handles privmsg
    -- TODO: identify how it handles multiple channel (Do we want list here) also is privmsg specified or Null?
    --  * It lists the bot's nick as a channel, which works for me.  !karma/etc are busted as a privmsg tho
    let chan  = Just $ T.decodeUtf8 $ whichChannel m
    let karma = parseInput conf nick msg

    case karma of
        (KarmaReply n (Just k)) -> do
            t <- liftIO getCurrentTime
            pool <- ask
            liftIO $ runSqlPool (addKarma t n nick user host chan k) pool
            return []
        _                       -> return []
rawKarma _ _ = return []

parseInput :: Config -> T.Text -> T.Text -> KarmaReply
parseInput config jnick jinput =
    -- Raw input, no json needed
    if filterBot config jnick
    then KarmaReply jnick Nothing
    else do
        -- Clean up the nick
        let nick = case parse nickDeFuzzifier "(stdin)" jnick of
                (Left _)  -> jnick
                (Right n) -> n

        -- Parse the message
        let karma = parse (karmaParse config) "(stdin)" jinput

        case karma of
            (Left _)  -> KarmaReply nick Nothing -- Failed to find a karma entry
            (Right k) -> do
                -- Scan the extracted karma expression for the nick
                let filteredKarma = filterKarma [nick, jnick] k

                if null filteredKarma
                then KarmaReply nick Nothing
                else KarmaReply nick (Just filteredKarma)

filterKarma :: [T.Text] -> [Karma] -> [Karma]
filterKarma n = filter (\Karma{kMessage=msg} -> msg `DL.notElem` n)


-- Load the config
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
