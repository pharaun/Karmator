{-# LANGUAGE OverloadedStrings #-}
module Karmator.Filter
    ( exactCommand
    , prefixMessage
    , whichChannel
    ) where

import qualified Data.ByteString as BS
import qualified Network.IRC as IRC

--
-- Simple Filters to start of, flat-matches
--

exactCommand :: BS.ByteString -> IRC.Message -> Bool
exactCommand c m = c == IRC.msg_command m

prefixMessage :: BS.ByteString -> IRC.Message -> Bool
prefixMessage c m = c `BS.isPrefixOf` (head $ tail $ IRC.msg_params m) -- TODO: unsafe head/tail, and does not support multi-channel privmsg

whichChannel :: IRC.Message -> BS.ByteString
whichChannel = head . IRC.msg_params -- TODO: unsafe head, and does not support multi-channel privmsg
