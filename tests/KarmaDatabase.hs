{-# LANGUAGE OverloadedStrings #-}

import qualified Database.Persist.Sqlite as P
import Database.Esqueleto
import Control.Monad.IO.Class

import Plugins.Karma.Database

main :: IO ()
main = P.runSqlite "test2.db" $ do

    vote1a <- allKarma KarmaReceived
    vote1b <- allKarma KarmaGiven

    vote3a <- partalKarma KarmaReceived ["nishbot", "doesnotexist"]
    vote3b <- partalKarma KarmaGiven ["nishbot", "doesnotexist"]

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

--    liftIO $ mapM print vote1a
--    liftIO $ mapM print vote1b
    liftIO $ print vote3a
    liftIO $ print vote3b
    liftIO $ print vote4a
    liftIO $ print vote4b
    liftIO $ print vote4c
    liftIO $ print vote4d
    liftIO $ print vote5a
    liftIO $ print vote5b
    liftIO $ print vote6a
    liftIO $ print vote6b
    liftIO $ print vote7a
    liftIO $ print vote7b
    liftIO $ print vote8a
    liftIO $ print vote8b
