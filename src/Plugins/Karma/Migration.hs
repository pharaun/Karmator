{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.Reader
import Data.List as DL
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Environment (getArgs)
import qualified Data.Text as T

-- Queries stuff
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class

-- Timezones
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson

-- V1 -> V2 schema (timestamp is string so we can mangle it into UTC)
share [mkPersist sqlSettings] [persistLowerCase|
Votes
    votedAt T.Text
    byWhomName T.Text
    forWhatName T.Text
    amount Int
    deriving Show

KarmaReceivedCount
    name T.Text
    up Int
    down Int
    side Int
    UniqueNameR name
    deriving Show

KarmaGivenCount
    name T.Text
    up Int
    down Int
    side Int
    UniqueNameG name
    deriving Show

OldKrc sql=oldkrc
    name T.Text
    up Int
    down Int
    side Int
    UniqueNameO name
    deriving Show

OkdKarma sql=oldkarma
    name T.Text
    normalized T.Text
    added Int
    subtracted Int
    UniqueNormalizedO normalized
    deriving Show
|]
-- Future Schema
--
-- voted_at - NOT NULL & UTCTime (Convert from LocalTime in PDT/PST to UTC) -- DONE
--
-- collate nocase == ascii, does not do unicode (utf8) nor case-folding,
-- normalization, may want 2 column (raw value, normalized value)
--
-- Also IRC handle can be special, the lower case and upper case isn't 100%
-- equivalent, there's special handling rules here, And apparently for ex
-- 'rtkbot' i can't /nick to 'RTKBOT' because its considered the same.
--

-- Streaming conversion from LocalTime to UTCTime
convertToUTC :: FilePath -> T.Text -> IO ()
convertToUTC tz conn = runSqlite conn $ do
    tzs <- liftIO $ getTimeZoneSeriesFromOlsonFile tz

    liftIO $ putStrLn "LocalTime, TimeZone, validLocalTime, redundantLocalTime, UTCTime"
    selectSource [] [] $$ CL.mapM_ (updateTime tzs)

updateTime :: (MonadIO m) => TimeZoneSeries -> Entity Votes -> ReaderT SqlBackend m ()
updateTime tzs (Entity {entityKey = key, entityVal=(Votes {votesVotedAt = textTime})}) = do
    let time      = read $ T.unpack textTime
    let valid     = isValidLocalTime tzs time
    let redundant = isRedundantLocalTime tzs time
    let zst       = localTimeToZoneSeriesTime tzs time
    let zone      = zoneSeriesTimeZone zst
    let utct      = zoneSeriesTimeToUTC zst

    -- Error reporting, go on ahead and report any invalid/redundant
    -- entries
    if (not valid) || redundant
    then liftIO $ putStrLn $ DL.intercalate "," [show time, show zone, show valid, show redundant, show utct]
    else return ()

    -- Update the row to have UTC timestamp
    update key [VotesVotedAt =. (T.pack $ show utct)]


main :: IO ()
main = do
    fileNames <- getArgs

    if DL.length fileNames /= 2
    then putStrLn "migration <localtime file '/etc/localtime/'> <database to be migrated>"
    else do
        putStrLn "Migration: LocalTime -> UTCTime"
        convertToUTC (DL.head fileNames) $ T.pack (DL.last fileNames)
