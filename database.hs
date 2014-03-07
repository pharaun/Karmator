{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import qualified Database.Esqueleto as E

import qualified Data.ByteString.Char8 as C8

-- Queries stuff
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)

import Types

-- Current Schema
share [mkPersist sqlOnlySettings] [persistLowerCase|
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

main :: IO ()
main = runSqlite "test2.db" $ do
    rawQuery "select * from Votes limit 1" [] $$ CL.mapM_ (liftIO . print)

    vote <- selectList [VotesForWhatName ==. "nishbot"] [LimitTo 1]
    liftIO $ print vote

    vote2 <- E.select $ E.from (\v -> E.where_ (v E.^. VotesForWhatName E.==. E.val "nishbot") >> E.limit 1 >> return v)
    liftIO $ Prelude.mapM_ (print . entityVal) vote2
