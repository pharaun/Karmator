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
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.Control

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

    vote4 <- allKarma
    vote5 <- allCountsReceived
    vote7 <- countsReceived ["nishbot", "doesnotexist"]
    vote8 <- topNDenormalizedReceived 3 desc
    vote9 <- rankingDenormalizedReceived "nishbot"
    vote10 <- countReceived "nishbot"
    vote11 <- statsForReceived 3 "nishbot"
    vote12 <- topNSideVotesDenormalizedReceived 3
    vote13 <- sideVotesRankingDenormalizedReceived "nishbot"
    vote14 <- sideVoteStats 3 "nishbot"

    liftIO $ Prelude.mapM_ (print . show) vote13
--    liftIO $ (print . show) vote11


-- TODO: extract (Value Text, Value Int) -> (Text, Int)
allKarma = select $ from (\v -> do
    orderBy [desc (v ^. KarmaReceivedCountName)]
    return (v ^. KarmaReceivedCountName, (v ^. KarmaReceivedCountUp) -. (v ^. KarmaReceivedCountDown))
    )

-- Genericify this to work both on Received or Given
allCountsReceived = select $ from (\v -> do
    orderBy [desc (v ^. KarmaReceivedCountName)]
    return (v ^. KarmaReceivedCountName, v ^. KarmaReceivedCountUp, v ^. KarmaReceivedCountDown, v ^. KarmaReceivedCountSide)
    )

-- TODO: extract the entity and fill out any non-existant entity with 0's for its values
-- Genericify this to work both on Received or Given
countsReceived [] = return []
countsReceived names = do
    select $ from (\v -> do
        orderBy [desc (v ^. KarmaReceivedCountName)]
        where_ (v ^. KarmaReceivedCountName `in_` valList names)
        return v
        )
--    return [(name, ret.get(name, (0, 0, 0))) for name in names]

-- Genericify this to work both on Received or Given
--def general_stats(session, count, from_whom): return stats_for(session, karma_in, count, from_whom)
--def giver_stats(session, count, from_whom): return stats_for(session, karma_out, count, from_whom)
--generalStatsReceived count nick =


-- Genericify this to work both on Received or Given
topNDenormalizedReceived lmt ord = do
    select $ from (\v -> do
        let cnt = ((v ^. KarmaReceivedCountUp) -. (v ^. KarmaReceivedCountDown))
        orderBy [ord cnt]
        limit lmt
        return (v ^. KarmaReceivedCountName, cnt)
        )

-- Genericify this to work both on Received or Given
rankingDenormalizedReceived
    :: (MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m, MonadLogger m)
    => T.Text
    -> SqlPersistT m [Value Int]
rankingDenormalizedReceived whom = do
    select $ from (\v -> do
        let sub = from $ (\c -> do
            where_ (c ^. KarmaReceivedCountName ==. val whom)
            return ((c ^. KarmaReceivedCountUp) -. (c ^.KarmaReceivedCountDown))
            )

        where_ (((v ^. KarmaReceivedCountUp) -. (v ^. KarmaReceivedCountDown)) >. sub_select sub)
        return $ count (v ^. KarmaReceivedCountName)
        )

-- Genericify this to work both on Received or Given
countReceived
    :: (MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m, MonadLogger m)
    => T.Text
    -> SqlPersistT m [Value Int]
countReceived whom = do
    select $ from (\v -> do
        where_ (v ^. KarmaReceivedCountName !=. val whom)
        return $ count (v ^. KarmaReceivedCountName) +. val 1
        )

-- Genericify this to work both on Received or Given
statsForReceived lmt whom = do
    desc_top <- topNDenormalizedReceived lmt desc
    asc_top  <- topNDenormalizedReceived lmt asc
    ranking  <- rankingDenormalizedReceived whom
    counting <- countReceived whom

    return (desc_top, asc_top, ranking, counting)


-- Genericify this to work both on Received or Given
topNSideVotesDenormalizedReceived lmt = do
    select $ from (\v -> do
        orderBy [desc (v ^. KarmaReceivedCountSide)]
        limit lmt
        return (v ^. KarmaReceivedCountName, v ^. KarmaReceivedCountSide)
        )

-- Genericify this to work both on Received or Given
sideVotesRankingDenormalizedReceived
    :: (MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m, MonadLogger m)
    => T.Text
    -> SqlPersistT m [Value Int]
sideVotesRankingDenormalizedReceived whom = do
    select $ from (\v -> do
        let sub = from $ (\c -> do
            where_ (c ^. KarmaReceivedCountName ==. val whom)
            return (c ^. KarmaReceivedCountSide)
            )

        where_ ((v ^. KarmaReceivedCountSide) >. sub_select sub)
        return $ count (v ^. KarmaReceivedCountName)
        )

-- Genericify this to work both on Received or Given
sideVoteStats lmt whom = do
    top_in    <- topNSideVotesDenormalizedReceived 3
--    top_out  <- topNSideVotesDenormalizedGiven 3
--    side_out  <- sideVotesRankingDenormalizedGiven whom
--    count_out <- countGiven whom
    side_in   <- sideVotesRankingDenormalizedReceived whom
    count_in  <- countReceived whom

--    return (top_in, top_out, side_out, count_out, side_in, count_in)
    return (top_in, side_in, count_in)


-- @interaction
--def add_karma(session, json_blob):
--    by_whom_name = json_blob['nick']
--    for kind in json_blob['karma']:
--        q = (vote.insert()
--             .values(by_whom_name=by_whom_name, for_what_name=kind['message'],
--                     amount=vote_amount_map[kind['karma_type']]))
--        session.execute(q)
