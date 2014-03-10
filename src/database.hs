{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P
import qualified Database.Persist.TH as P
import Database.Esqueleto

import Data.Text (Text)
import qualified Data.Text as T

-- Current Schema
P.share [P.mkPersist P.sqlOnlySettings] [P.persistLowerCase|
Votes
    votedAt UTCTime
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
|]

main :: IO ()
main = P.runSqlite "test2.db" $ do
    rawQuery "select * from Votes limit 1" [] $$ CL.mapM_ (liftIO . print)

    vote <- P.selectList [VotesForWhatName P.==. "nishbot"] [P.LimitTo 1]
    liftIO $ print vote

    vote2 <- select $ from (\v -> where_ (v ^. VotesForWhatName ==. val "nishbot") >> limit 1 >> return v)
    liftIO $ Prelude.mapM_ (print . entityVal) vote2
