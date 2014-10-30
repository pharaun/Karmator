{-# LANGUAGE OverloadedStrings #-}
module Plugins.Generic
    ( pingMatch
    , ping

    , motdMatch
    , motdJoin

    , inviteMatch
    , inviteJoin

    , uptimeMatch
    , uptime
    ) where

import Data.List
import System.Time
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Karmator.Types
import Karmator.Filter
import qualified Network.IRC as IRC



--
-- Ping
--
pingMatch :: BotEvent -> Bool
pingMatch = exactCommand "PING"

-- TODO: Unsafe head
ping :: BotEvent -> Maybe BotCommand
ping (EMessage _ m) = Just $ CMessage $ IRC.pong $ head $ IRC.msg_params m
ping _ = Nothing


--
-- Motd Join
-- TODO: add support for "auth ping/pong" before registering/joining channels
-- TODO: too many channels will cause this to be too long and truncated.
-- Need to split it and emit multiple joins as needed
--
motdMatch n m = exactCommand "004" m && networkMatch n m
motdJoin cs _ = Just $ CMessage $ IRC.joinChan $ BS.intercalate "," cs




--
-- Invite
--
inviteMatch             = exactCommand "INVITE"
inviteJoin (EMessage _ m) = Just $ CMessage $ IRC.joinChan $ head $ tail $ IRC.msg_params m


--
-- Uptime
--
uptimeMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!uptime")
uptime t m  = do
    now <- liftIO $ getClockTime
    return $ Just $ CMessage $ IRC.privmsg (whichChannel m) (C8.pack $ pretty $ diffClockTimes now t)

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
