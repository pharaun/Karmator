{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import qualified Data.ByteString.Char8 as C8

import Types

-- Old Schema
-- CREATE TABLE "oldkrc" (
--     name VARCHAR PRIMARY KEY COLLATE nocase,
--     up INTEGER NOT NULL,
--     down INTEGER NOT NULL,
--     side INTEGER NOT NULL
-- );
-- CREATE TABLE oldkarma(
--   id INT,
--   name TEXT,
--   normalized TEXT primary key,
--   added INT,
--   subtracted INT
-- );
-- CREATE TABLE sidevotes (name text, value int);


-- Current Schema
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"] [persistLowerCase|
Votes
    votedAt LocalTime
    byWhomName Text Maybe
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
|]
--2013-07-26 14:48:33.819647
--CREATE TABLE votes (
--    voted_at DATETIME,
--    by_whom_name VARCHAR COLLATE nocase,
--    for_what_name VARCHAR NOT NULL COLLATE nocase,
--    amount INTEGER NOT NULL
--);
--
--CREATE TABLE karma_received_count (
--    name VARCHAR PRIMARY KEY COLLATE nocase,
--    up INTEGER NOT NULL,
--    down INTEGER NOT NULL,
--    side INTEGER NOT NULL
--);
--
--CREATE TABLE karma_given_count (
--    name VARCHAR PRIMARY KEY COLLATE nocase,
--    up INTEGER NOT NULL,
--    down INTEGER NOT NULL,
--    side INTEGER NOT NULL
--);

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

main :: IO ()
main = runSqlite "test2.db" $ do
    printMigration migrateAll
