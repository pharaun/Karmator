{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Data.List as DL
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Database.Persist.Sqlite as P
import qualified Database.Persist.TH as P
import Database.Esqueleto

import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (FilePath)

-- Queries stuff
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class

-- Timezones
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson

import Types

-- Current Schema
P.share [P.mkPersist P.sqlOnlySettings] [P.persistLowerCase|
Votes
    votedAt LocalTime
    byWhomName Text
    forWhatName Text
    amount Int
    deriving Show

KarmaReceivedCount
    name Text
    up Int
    down Int
    side Int
    UniqueNameR name
    deriving Show

KarmaGivenCount
    name Text
    up Int
    down Int
    side Int
    UniqueNameG name
    deriving Show

OldKrc sql=oldkrc
    name Text
    up Int
    down Int
    side Int
    UniqueNameO name
    deriving Show

OkdKarma sql=oldkarma
    name Text
    normalized Text
    added Int
    subtracted Int
    UniqueNormalizedO normalized
    deriving Show
|]

-- Future Schema
--
-- voted_at - NOT NULL & UTCTime (Convert from LocalTime in PDT/PST to UTC)
-- by_whom_name - NOT NULL (one entry, remove)
-- for_what_name - NOT NULL (three entry, remove)
-- name - NOT NULL (one entry in both cases, remove)
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
convertToUTC tz conn = P.runSqlite conn $ do
    tzs <- liftIO $ getTimeZoneSeriesFromOlsonFile tz

    liftIO $ putStrLn "LocalTime, TimeZone, validLocalTime, redundantLocalTime, UTCTime"

    P.selectSource [] [] $$ CL.mapM_ (updateTime tzs)

updateTime :: MonadIO m => TimeZoneSeries -> Entity Votes -> m ()
updateTime tzs (Entity {entityKey = key, entityVal=(Votes {votesVotedAt = time})}) = do
    let valid     = isValidLocalTime tzs time
    let redundant = isRedundantLocalTime tzs time
    let zst       = localTimeToZoneSeriesTime tzs time
    let zone      = zoneSeriesTimeZone zst
    let utc       = zoneSeriesTimeToUTC zst

    -- Dump any that isn't valid or are redundant
    if (not valid) || redundant
    then liftIO $ putStrLn $ DL.intercalate "," [show time, show zone, show valid, show redundant, show utc]
    else return ()

main :: IO ()
main = convertToUTC "/etc/localtime" $ T.pack "test2.db"
--main = P.runSqlite "test2.db" $ do
--    rawQuery "select * from Votes limit 1" [] $$ CL.mapM_ (liftIO . print)
--
--    vote <- P.selectList [VotesForWhatName P.==. "nishbot"] [P.LimitTo 1]
--    liftIO $ print vote
--
--    vote2 <- select $ from (\v -> where_ (v ^. VotesForWhatName ==. val "nishbot") >> limit 1 >> return v)
--    liftIO $ Prelude.mapM_ (print . entityVal) vote2
