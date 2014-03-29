{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P
import qualified Database.Persist.TH as P
import Database.Esqueleto

import Data.Text (Text)
import qualified Data.Text as T

import Data.Time.Clock (UTCTime)
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class

import Data.Int

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

-- Support both type of karma; received and given
data KarmaTable = KarmaReceived | KarmaGiven

-- Karma*Name
karmaReceivedName t = t ^. KarmaReceivedCountName
karmaGivenName t    = t ^. KarmaGivenCountName

-- Karma*Up
karmaReceivedUp t = t ^. KarmaReceivedCountUp
karmaGivenUp t    = t ^. KarmaGivenCountUp

-- Karma*Down
karmaReceivedDown t = t ^. KarmaReceivedCountDown
karmaGivenDown t    = t ^. KarmaGivenCountDown

-- Karma*Side
karmaReceivedSide t = t ^. KarmaReceivedCountSide
karmaGivenSide t    = t ^. KarmaGivenCountSide

-- Karma*Total
karmaReceivedTotal t = karmaReceivedUp t -. karmaReceivedDown t
karmaGivenTotal t    = karmaGivenUp t -. karmaGivenDown t

-- Queries
allKarma KarmaReceived = allKarmaT karmaReceivedName karmaReceivedTotal
allKarma KarmaGiven    = allKarmaT karmaGivenName karmaGivenTotal

allCounts KarmaReceived = allCountsT karmaReceivedName karmaReceivedUp karmaReceivedDown karmaReceivedSide
allCounts KarmaGiven    = allCountsT karmaGivenName karmaGivenUp karmaGivenDown karmaGivenSide

counts KarmaReceived = countsT karmaReceivedName karmaReceivedUp karmaReceivedDown karmaReceivedSide
counts KarmaGiven    = countsT karmaGivenName karmaGivenUp karmaGivenDown karmaGivenSide

topNDenormalized KarmaReceived = topNDenormalizedT karmaReceivedName karmaReceivedTotal
topNDenormalized KarmaGiven    = topNDenormalizedT karmaGivenName karmaGivenTotal

rankingDenormalized KarmaReceived = rankingDenormalizedT karmaReceivedName karmaReceivedTotal
rankingDenormalized KarmaGiven    = rankingDenormalizedT karmaGivenName karmaGivenTotal

countK KarmaReceived = countT karmaReceivedName
countK KarmaGiven    = countT karmaGivenName

topNSideVotesDenormalized KarmaReceived lmt = topNDenormalizedT karmaReceivedName karmaReceivedSide lmt desc
topNSideVotesDenormalized KarmaGiven lmt    = topNDenormalizedT karmaGivenName karmaGivenSide lmt desc

sideVotesRankingDenormalized KarmaReceived = rankingDenormalizedT karmaReceivedName karmaReceivedSide
sideVotesRankingDenormalized KarmaGiven    = rankingDenormalizedT karmaGivenName karmaGivenSide


-- TODO: extract (Value Text, Value Int) -> (Text, Int)
allKarmaT karmaName karmaTotal = select $ from (\v -> do
    orderBy [desc (karmaName v)]
    return (karmaName v, karmaTotal v)
    )

allCountsT karmaName karmaUp karmaDown karmaSide = select $ from (\v -> do
    orderBy [desc (karmaName v)]
    return (karmaName v, karmaUp v, karmaDown v, karmaSide v)
    )

-- TODO: extract the entity and fill out any non-existant entity with 0's for its values
countsT _ _ _ _ [] = return []
countsT karmaName karmaUp karmaDown karmaSide names = do
    select $ from (\v -> do
        orderBy [desc (karmaName v)]
        where_ ((karmaName v) `in_` valList names)
        return (karmaName v, (karmaUp v, karmaDown v, karmaSide v))
        )
--    return [(name, ret.get(name, (0, 0, 0))) for name in names]

-- karmaTotal is also good for karmaSide
topNDenormalizedT karmaName karmaTotal lmt ord =
    select $ from (\v -> do
        orderBy [ord (karmaTotal v)]
        limit lmt
        return (karmaName v, karmaTotal v)
        )

-- karmaTotal is also good for karmaSide
-- TODO: i think it needs to be a +1 here
rankingDenormalizedT karmaName karmaTotal whom =
    select $ from (\v -> do
        let sub = from $ (\c -> do
            where_ (karmaName c ==. val whom)
            return $ karmaTotal c
            )
        where_ (karmaTotal v >. sub_select sub)
        return $ count (karmaName v) :: SqlQuery (SqlExpr (Value Int))
        )

countT karmaName whom =
    select $ from (\v -> do
        where_ (karmaName v !=. val whom)
        return $ count (karmaName v) +. val 1 :: SqlQuery (SqlExpr (Value Int))
        )


-- @interaction
--def add_karma(session, json_blob):
--    by_whom_name = json_blob['nick']
--    for kind in json_blob['karma']:
--        q = (vote.insert()
--             .values(by_whom_name=by_whom_name, for_what_name=kind['message'],
--                     amount=vote_amount_map[kind['karma_type']]))
--        session.execute(q)



main :: IO ()
main = P.runSqlite "test2.db" $ do

    vote1a <- allKarma KarmaReceived
    vote1b <- allKarma KarmaGiven

    vote2a <- allCounts KarmaReceived
    vote2b <- allCounts KarmaGiven

    vote3a <- counts KarmaReceived ["nishbot", "doesnotexist"]
    vote3b <- counts KarmaGiven ["nishbot", "doesnotexist"]

    vote4a <- topNDenormalized KarmaReceived 3 desc
    vote4b <- topNDenormalized KarmaGiven 3 desc
    vote4c <- topNDenormalized KarmaReceived 3 asc
    vote4d <- topNDenormalized KarmaGiven 3 asc

    vote5a <- rankingDenormalized KarmaReceived "nishbot"
    vote5b <- rankingDenormalized KarmaGiven "nishbot"

    vote6a <- countK KarmaReceived "nishbot"
    vote6b <- countK KarmaGiven "nishbot"

    vote7a <- topNSideVotesDenormalized KarmaReceived 3
    vote7b <- topNSideVotesDenormalized KarmaGiven 3

    vote8a <- sideVotesRankingDenormalized KarmaReceived "nishbot"
    vote8b <- sideVotesRankingDenormalized KarmaGiven "nishbot"

    liftIO $ Prelude.mapM_ (print . show) vote8a
--    liftIO $ (print . show) vote11
