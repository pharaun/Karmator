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


karmaSidevotesMatch :: BotEvent -> Bool
karmaSidevotesMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!sidevotes")

karmaSidevotes :: MonadIO m => Config -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
karmaSidevotes _ m@(EMessage _ _) = do
    let nick     = T.decodeUtf8 $ nickContent m
    let countMax = 3

    pool <- ask

    (received, given) <- liftIO $ flip runSqlPool pool $ do
            a <- topNSideVotesDenormalized KarmaReceived countMax
            b <- topNSideVotesDenormalized KarmaGiven countMax
            return (a, b)

    return [CMessage $ IRC.privmsg (whichChannel m) (
        if null received || null given
        then "There is no karma values recorded in the database!"
        else BL.toStrict $ TL.encodeUtf8 (
            format (stext % ", most sidevotes received: " % text % ". most sidevotes given: " % text % ". ")
                   nick
                   (renderTotalKarma received)
                   (renderTotalKarma given)
            )
        )]
karmaSidevotes _ _ = return []


karmaMatch :: BotEvent -> Bool
karmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!karma")

karma :: MonadIO m => Config -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
karma conf m = do
    pool <- ask
    karmaStats conf pool KarmaReceived 3 False m


karmaGiversMatch :: BotEvent -> Bool
karmaGiversMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!givers")

karmaGivers :: MonadIO m => Config -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
karmaGivers conf m = do
    pool <- ask
    karmaStats conf pool KarmaGiven 3 True m


karmaStats :: (MonadIO m) => Config -> ConnectionPool -> KarmaTable -> Int64 -> Bool -> BotEvent -> m [BotCommand]
karmaStats conf pool karmaType countMax givers m@(EMessage _ _) =
    case parse (karmaCommandParse conf) "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)   -> return [CMessage $ IRC.privmsg (whichChannel m) "Karma command parse failed"]
        (Right []) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            (topNDesc, topNAsc) <- liftIO $ flip runSqlPool pool $ do
                    a <- topNDenormalized karmaType countMax desc
                    b <- topNDenormalized karmaType countMax asc
                    return (a, b)

            return [CMessage $ IRC.privmsg (whichChannel m) (
                if null topNDesc || null topNAsc
                then "There is no karma values recorded in the database!"
                else BL.toStrict $ TL.encodeUtf8 (
                    format (if givers
                            then stext % ", most positive: " % text % ". most negative: " % text % "."
                            else stext % ", highest karma: " % text % ". lowest karma: " % text % ".")
                           nick
                           (renderTotalKarma topNDesc)
                           (renderTotalKarma topNAsc)
                    )
                )]
        (Right x)  -> do
            let nick  = T.decodeUtf8 $ nickContent m
            count <- liftIO $ runSqlPool (partalKarma karmaType x) pool
            return [CMessage $ IRC.privmsg (whichChannel m) $ BL.toStrict $ TL.encodeUtf8 (
                format (stext % ": " % text)
                    nick
                    (renderAllKarma count)
                )]
karmaStats _ _ _ _ _ _ = return []


karmaRankMatch :: BotEvent -> Bool
karmaRankMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!rank")


karmaRank :: MonadIO m => Config -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
karmaRank conf m@(EMessage _ _) =
    case parse (karmaCommandParse conf) "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)       -> return [CMessage $ IRC.privmsg (whichChannel m) "Karma command parse failed"]
        (Right [])     -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool False nick nick "Your"
            return [CMessage $ IRC.privmsg (whichChannel m) msg]
        (Right [x]) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool False nick x x
            return [CMessage $ IRC.privmsg (whichChannel m) msg]
        (Right _)      -> return [CMessage $ IRC.privmsg (whichChannel m) "Can only rank one karma entry at a time!"]
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

    return $ BL.toStrict $ TL.encodeUtf8 $ format (stext % ", " % text) nick (
        case (recvRank, giveRank) of
            (Nothing, Nothing) -> "No ranking available"
            (Just r,  Nothing) -> format (stext % " rank is " % int % " of " % int % " in receiving.") target r recvCount
            (Nothing, Just g)  -> format (stext % " rank is " % int % " of " % int % " in giving.") target g giveCount
            (Just r,  Just g)  -> format (stext % " rank is " % int % " of " % int % " in receiving and " % int % " of " % int % " in giving.") target r recvCount g giveCount
        )


karmaSidevotesRankMatch :: BotEvent -> Bool
karmaSidevotesRankMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!ranksidevote")

karmaSidevotesRank :: MonadIO m => Config -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
karmaSidevotesRank conf m@(EMessage _ _) =
    case parse (karmaCommandParse conf) "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)       -> return [CMessage $ IRC.privmsg (whichChannel m) "Karma command parse failed"]
        (Right [])     -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool True nick nick "Your"
            return [CMessage $ IRC.privmsg (whichChannel m) msg]
        (Right [x]) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            pool <- ask
            msg <- renderRank pool True nick x x
            return [CMessage $ IRC.privmsg (whichChannel m) msg]
        (Right _)      -> return [CMessage $ IRC.privmsg (whichChannel m) "Can only rank one karma entry at a time!"]
karmaSidevotesRank _ _ = return []


-- This takes care of sulping all irc messages to stuff into the karma parser
rawKarmaMatch :: BotEvent -> Bool
rawKarmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (not . prefixMessage "!")

rawKarma :: MonadIO m => Config -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
rawKarma conf m@(EMessage _ _) = do
    -- ByteString -> utf8
    -- TODO: error handling
    let msg   = T.decodeUtf8 $ messageContent m
    let nick  = T.decodeUtf8 $ nickContent m
    -- TODO: not for certain if we want to preserve '~' in username or not
    let user  = T.decodeUtf8 <$> userNameContent m
    let host  = T.decodeUtf8 <$> hostMaskContent m
    let karma = parseInput conf nick msg

    case karma of
        (KarmaReply n (Just k)) -> do
            t <- liftIO getCurrentTime
            pool <- ask
            liftIO $ runSqlPool (addKarma t n nick user host k) pool
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

        -- Force the Parser to invoke Read on the Partial/KarmaTypes
        partialKarma <- get c "karma_parsing" "partial"
        totalKarma <- get c "karma_parsing" "total"

        -- Braces
        open <- get c "karma_parsing" "open_brace"
        close <- get c "karma_parsing" "close_brace"

        -- Braces
        commandOpen <- get c "command_parsing" "open_brace"
        commandClose <- get c "command_parsing" "close_brace"

        return $ Config strictMatch prefixMatch partialKarma totalKarma open close commandOpen commandClose)

    case config of
        Left cperr   -> error $ show cperr
        Right config -> return config
