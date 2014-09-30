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

    liftIO $ Prelude.mapM_ (print . show) vote1a
    liftIO $ Prelude.mapM_ (print . show) vote1b
    liftIO $ Prelude.mapM_ (print . show) vote3a
    liftIO $ Prelude.mapM_ (print . show) vote3b
    liftIO $ Prelude.mapM_ (print . show) vote4a
    liftIO $ Prelude.mapM_ (print . show) vote4b
    liftIO $ Prelude.mapM_ (print . show) vote4c
    liftIO $ Prelude.mapM_ (print . show) vote4d
    liftIO $ Prelude.mapM_ (print . show) vote5a
    liftIO $ Prelude.mapM_ (print . show) vote5b
    liftIO $ Prelude.mapM_ (print . show) vote6a
    liftIO $ Prelude.mapM_ (print . show) vote6b
    liftIO $ Prelude.mapM_ (print . show) vote7a
    liftIO $ Prelude.mapM_ (print . show) vote7b
    liftIO $ Prelude.mapM_ (print . show) vote8a
    liftIO $ Prelude.mapM_ (print . show) vote8b
