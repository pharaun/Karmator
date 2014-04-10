{-# LANGUAGE OverloadedStrings #-}
module Karmator.Filter
    ( exactCommand
    ) where

import qualified Data.ByteString as BS
import qualified Network.IRC as IRC

--
-- Simple Filters to start of, flat-matches
--

exactCommand :: BS.ByteString -> IRC.Message -> Bool
exactCommand c m = c == IRC.msg_command m

--    return $ if "PRIVMSG" /= IRC.msg_command msg
--    then Nothing
--    else if "!uptime" `BS.isPrefixOf` (head $ tail $ IRC.msg_params msg) -- TODO: unsafe head/tail
--         then Just $ IRC.privmsg "#levchins" (C8.pack $ pretty $ diffClockTimes now t)
--         else Nothing
