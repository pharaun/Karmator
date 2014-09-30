{-# LANGUAGE OverloadedStrings #-}
module Plugins.Karma
    ( rawKarmaMatch
    , rawKarma

    , getKarmaConfig
    ) where

import Control.Monad.Error
import Control.Monad.IO.Class
import Data.List
import qualified Data.ByteString.Char8 as C8

import System.IO (stdin, stdout, stderr, hSetBuffering, BufferMode(..), hPrint, hIsEOF, Handle, hSetEncoding, utf8)
import Control.Monad (mzero, unless, liftM)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.List as DL

-- Parsec
import Text.Parsec

-- Karma module
import Plugins.Karma.Karma
import Plugins.Karma.Types

-- Configuration
import Data.ConfigFile
import Prelude hiding (readFile)

import Karmator.Types
import Karmator.Filter
import qualified Network.IRC as IRC

import Database.Persist.Sql hiding (get)

import Data.Text.Encoding

--uptimeMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!uptime")
-- TODO
--  Support these handles:
--      - !karma
--      - !karma foo bar
--      - !karma "foo bar" "coo"
--      - !karmagivers
--      - !sidevotes
--      - !karmatorjoin
--      - !karmatorleave
--      - !IRC_INVITE (invite command)


-- This takes care of sulping all irc messages to stuff into the karma parser
rawKarmaMatch :: BotEvent -> Bool
rawKarmaMatch = liftM2 (&&) (exactCommand "PRIVMSG") (not . prefixMessage "!")

rawKarma :: (MonadIO m) => Config -> ConnectionPool -> BotEvent -> m (Maybe BotCommand)
rawKarma conf pool m@(EMessage _) = do
    -- ByteString -> utf8
    -- TODO: error handling
    let msg   = decodeUtf8 $ messageContent m
    let nick  = decodeUtf8 $ nickContent m
    let karma = parseInput conf nick msg

    liftIO $ print karma

    return $ Nothing

rawKarma _ _ _ = return Nothing


parseInput :: Config -> T.Text -> T.Text -> KarmaReply
parseInput config jnick jinput = do
    -- Raw input, no json needed
    if filterBot config $ jnick
    then KarmaReply (Just jnick) Nothing Nothing
    else do
        -- Clean up the nick
        let nick = case parse nickDeFuzzifier "(stdin)" $ jnick of
                (Left _)  -> jnick
                (Right n) -> n

        -- Parse the message
        let karma = parse (karmaParse config) "(stdin)" $ jinput

        case karma of
            (Left _)  -> KarmaReply (Just nick) Nothing Nothing -- Failed to find a karma entry
            (Right k) -> do
                -- Scan the extracted karma expression for the nick
                let filteredKarma = filterKarma [nick, jnick] k

                KarmaReply (Just nick) (Just filteredKarma) Nothing

filterKarma :: [T.Text] -> [Karma] -> [Karma]
filterKarma n = filter (\Karma{kMessage=msg} -> msg `DL.notElem` n)


-- Load the config
getKarmaConfig :: FilePath -> IO Config
getKarmaConfig conf = do
    config <- runErrorT (do
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
