{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Plugins.Generic
    ( uptimeMatch
    , uptime

    , versionMatch
    , version
    , versionText
    ) where

import Data.List
import System.Time
import System.Locale
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as C8

import Karmator.Types
import Karmator.Filter
import qualified Network.IRC as IRC

-- Versions out of the cabal file
import qualified Paths_karmator as PK
import Data.Version (showVersion)
import Language.Haskell.TH


--
-- Uptime
--
uptimeMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!uptime")
uptime t m  = do
    now <- liftIO getClockTime
    return [CMessage $ IRC.privmsg (whichChannel m) (C8.pack $ pretty $ diffClockTimes now t)]

--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s


--
-- Version
--
versionMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!version")
version m = [CMessage $ IRC.privmsg (whichChannel m) $ C8.pack versionText]

versionText = concat
    [ "Version: "
    , showVersion PK.version
    , " - Build Date: "
    -- TODO: figure out why the last character is eated here
    , $((stringE . init) =<< (runIO ((return . (formatCalendarTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S %Z ")) =<< (toCalendarTime =<< getClockTime))))
    ]
