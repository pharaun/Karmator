{-# LANGUAGE OverloadedStrings #-}
module Plugins.Karma
    ( karmaMatch
    , karma

    , karmaSidevotesMatch
    , karmaSidevotes

    , karmaGiversMatch
    , karmaGivers

    , rawKarmaMatch
    , rawKarma

    , getKarmaConfig
    ) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List

import System.IO (stdin, stdout, stderr, hSetBuffering, BufferMode(..), hPrint, hIsEOF, Handle, hSetEncoding, utf8)
import Control.Monad (mzero, unless, liftM)
import Data.Int

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.List as DL

import Data.Text.Lazy.Builder (Builder)
import Formatting

-- Parsec
import Text.Parsec

-- Karma module
import Plugins.Karma.Karma
import Plugins.Karma.Types
import Plugins.Karma.Database

-- Configuration
import Data.ConfigFile
import Prelude hiding (readFile)

import Karmator.Types
import Karmator.Filter
import qualified Network.IRC as IRC

import Database.Persist.Sql hiding (get)


import Data.Time.Clock


karmaSidevotesMatch :: BotEvent -> Bool
karmaSidevotesMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!sidevotes")

karmaSidevotes :: (MonadIO m) => Config -> ConnectionPool -> BotEvent -> m (Maybe BotCommand)
karmaSidevotes conf pool m@(EMessage _) = undefined
karmaSidevotes _ _ _ = return Nothing


karmaMatch :: BotEvent -> Bool
karmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!karma")

karma :: (MonadIO m) => Config -> ConnectionPool -> BotEvent -> m (Maybe BotCommand)
karma conf pool m = karmaStats conf pool KarmaReceived 3 m


karmaGiversMatch :: BotEvent -> Bool
karmaGiversMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!givers")

-- TODO: return '%s, most positive: %s. most negative: %s. your rank is %d of %d in positivity.'
karmaGivers :: (MonadIO m) => Config -> ConnectionPool -> BotEvent -> m (Maybe BotCommand)
karmaGivers conf pool m = karmaStats conf pool KarmaGiven 3 m


-- TODO: different formatting string for the general case
karmaStats :: (MonadIO m) => Config -> ConnectionPool -> KarmaTable -> Int64 -> BotEvent -> m (Maybe BotCommand)
karmaStats conf pool karmaType countMax m@(EMessage _) =
    case (parse (karmaCommandParse conf) "(irc)" $ T.decodeUtf8 $ messageContent m) of
        (Left _)   -> return $ Just $ CMessage $ IRC.privmsg (whichChannel m) "Karma command parse failed"
        (Right []) -> do
            let nick  = T.decodeUtf8 $ nickContent m
            (topNDesc, topNAsc, ranking, total) <- liftIO $ flip runSqlPool pool (do
                    a <- topNDenormalized karmaType countMax desc
                    b <- topNDenormalized karmaType countMax asc
                    c <- rankingDenormalized karmaType nick
                    d <- countK KarmaGiven nick
                    return (a, b, c, d)
                )
            return $ Just $ CMessage $ IRC.privmsg (whichChannel m) $ BL.toStrict $ TL.encodeUtf8 (
                format (stext % ", highest karma: " % stext % ". lowest karma: " % stext % ". your rank is " % int % " of " % int % ".")
                       nick
                       (T.intercalate "; " $ map (\(n, k) -> T.concat [n, " (", T.pack $ show k, ")"]) topNDesc)
                       (T.intercalate "; " $ map (\(n, k) -> T.concat [n, " (", T.pack $ show k, ")"]) topNAsc)
                       (ranking + 1) -- TODO: ranking isn't right
                       total
                )
        (Right x)  -> do
            -- TODO: have this fill in zeros for values that are not returned
            let nick  = T.decodeUtf8 $ nickContent m
            count <- liftIO $ runSqlPool (partalKarma karmaType x) pool
            return $ Just $ CMessage $ IRC.privmsg (whichChannel m) $ BL.toStrict $ TL.encodeUtf8 (
                format (stext % ": " % text)
                    nick
                    (TL.intercalate "; " $ map (\(name, p, n, s) -> format (stext % ", " % int % " (" % int % "++/" % int % "--/" % int % "+-)") name (p-n) p n s) count)
                )
karmaStats _ _ _ _ _ = return Nothing



-- This takes care of sulping all irc messages to stuff into the karma parser
rawKarmaMatch :: BotEvent -> Bool
rawKarmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (not . prefixMessage "!")

rawKarma :: (MonadIO m) => Config -> ConnectionPool -> BotEvent -> m (Maybe BotCommand)
rawKarma conf pool m@(EMessage _) = do
    -- ByteString -> utf8
    -- TODO: error handling
    let msg   = T.decodeUtf8 $ messageContent m
    let nick  = T.decodeUtf8 $ nickContent m
    let karma = parseInput conf nick msg

    case karma of
        (KarmaReply n (Just k)) -> do
            t <- liftIO $ getCurrentTime
            liftIO $ runSqlPool (addKarma t n k) pool
            return $ Nothing
        otherwise -> return $ Nothing
rawKarma _ _ _ = return Nothing

parseInput :: Config -> T.Text -> T.Text -> KarmaReply
parseInput config jnick jinput = do
    -- Raw input, no json needed
    if filterBot config $ jnick
    then KarmaReply jnick Nothing
    else do
        -- Clean up the nick
        let nick = case parse nickDeFuzzifier "(stdin)" $ jnick of
                (Left _)  -> jnick
                (Right n) -> n

        -- Parse the message
        let karma = parse (karmaParse config) "(stdin)" $ jinput

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

        return $ Config strictMatch prefixMatch partialKarma totalKarma open close)

    case config of
        Left cperr   -> error $ show cperr
        Right config -> return config
