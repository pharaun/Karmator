{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Plugins.Karma.Database
    ( KarmaTable(..)
    , allKarma
    , partalKarma
    , topNDenormalized
    , topNSideVotesDenormalized
    , countK
    , rankingDenormalized
    , sideVotesRankingDenormalized

    , addKarma

    -- re-exports
    , desc
    , asc

    -- Test database migration
    , migrateAll
    ) where

import Data.List (unionBy)
import Data.Time.Clock (UTCTime)
import Database.Esqueleto
import Data.Text (Text, toCaseFold)
import qualified Database.Persist.TH as P

import Plugins.Karma.Types (Karma(kMessage,kType), KarmaType(..))


-- Current Schema
P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [P.persistLowerCase|
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
allKarma KarmaReceived = allKarmaT karmaReceivedName karmaReceivedUp karmaReceivedDown karmaReceivedSide
allKarma KarmaGiven    = allKarmaT karmaGivenName karmaGivenUp karmaGivenDown karmaGivenSide

partalKarma KarmaReceived = partalKarmaT karmaReceivedName karmaReceivedUp karmaReceivedDown karmaReceivedSide
partalKarma KarmaGiven    = partalKarmaT karmaGivenName karmaGivenUp karmaGivenDown karmaGivenSide

topNDenormalized KarmaReceived = topNDenormalizedT karmaReceivedName karmaReceivedTotal
topNDenormalized KarmaGiven    = topNDenormalizedT karmaGivenName karmaGivenTotal

topNSideVotesDenormalized KarmaReceived lmt = topNDenormalizedT karmaReceivedName karmaReceivedSide lmt desc
topNSideVotesDenormalized KarmaGiven lmt    = topNDenormalizedT karmaGivenName karmaGivenSide lmt desc

countK KarmaReceived = countT karmaReceivedName
countK KarmaGiven    = countT karmaGivenName

rankingDenormalized KarmaReceived = rankingDenormalizedT karmaReceivedName karmaReceivedTotal
rankingDenormalized KarmaGiven    = rankingDenormalizedT karmaGivenName karmaGivenTotal

sideVotesRankingDenormalized KarmaReceived = rankingDenormalizedT karmaReceivedName karmaReceivedSide
sideVotesRankingDenormalized KarmaGiven    = rankingDenormalizedT karmaGivenName karmaGivenSide

---------------------------------------------------------------------------------------------
allKarmaT karmaName karmaUp karmaDown karmaSide = do
    r <- select $ from (\v -> do
            orderBy [desc (karmaName v)]
            return (karmaName v, karmaUp v, karmaDown v, karmaSide v)
            )
    return $ map (\(n, u, d, s) -> (unValue n, unValue u, unValue d, unValue s)) r

partalKarmaT _ _ _ _ [] = return []
partalKarmaT karmaName karmaUp karmaDown karmaSide names = do
    r <- select $ from (\v -> do
            orderBy [desc (karmaName v)]
            where_ ((karmaName v) `in_` valList names)
            return (karmaName v, karmaUp v, karmaDown v, karmaSide v)
            )
    -- TODO: should return it in the same order that the names were yielded
    return $ unionBy (\(a, _, _, _) (b, _, _, _) -> toCaseFold a == toCaseFold b) (map (\(n, u, d, s) -> (unValue n, unValue u, unValue d, unValue s)) r) (map (\n -> (n, 0, 0, 0)) names)

-- karmaTotal is also good for karmaSide
topNDenormalizedT karmaName karmaTotal lmt ord = do
    r <- select $ from (\v -> do
            orderBy [ord (karmaTotal v)]
            limit lmt
            return (karmaName v, karmaTotal v)
            )
    return $ map (\(n, k) -> (unValue n, unValue k)) r

countT karmaName whom = do
    r <- select $ from (\v -> do
--            where_ (karmaName v !=. val whom)
            return $ count (karmaName v) :: SqlQuery (SqlExpr (Value Int))
            )
    return $ unValue $ head r -- TODO: unsafe head

-- karmaTotal is also good for karmaSide
rankingDenormalizedT karmaName karmaTotal whom = do
    r <- select $
            return $
                case_
                    [ when_
                        (exists $ from $ \v -> where_ (karmaName v ==. val whom))
                      then_
                        (sub_select $ from $ \v -> do
                            let sub =
                                    from $ \c -> do
                                    where_ (karmaName c ==. val whom)
                                    return $ karmaTotal c
                            where_ (karmaTotal v >. sub_select sub)
                            return $ just (count (karmaName v) +. val 1) :: SqlQuery (SqlExpr (Value (Maybe Int)))
                            ) ]
                    (else_ $ nothing)
    return $ unValue $ head r -- TODO: unsafe head

addKarma timestamp karmaName karmaValues =
    insertMany_ (map (\v -> Votes timestamp karmaName (kMessage v) (typeToInt (kType v))) karmaValues)
  where
    typeToInt Upvote   = 1
    typeToInt Sidevote = 0
    typeToInt Downvote = (-1)
