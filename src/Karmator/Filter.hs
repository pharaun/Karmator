{-# LANGUAGE OverloadedStrings #-}
module Karmator.Filter
    ( exactCommand
    , prefixMessage
    , whichChannel
    ) where

import qualified Data.ByteString as BS
import qualified Network.IRC as IRC
import Karmator.Types

--
-- Simple Filters to start of, flat-matches
--

exactCommand :: BS.ByteString -> BotEvent -> Bool
exactCommand c (EMessage m) = c == IRC.msg_command m
exactCommand _ _            = False

prefixMessage :: BS.ByteString -> BotEvent -> Bool
prefixMessage c (EMessage m) = c `BS.isPrefixOf` (head $ tail $ IRC.msg_params m) -- TODO: unsafe head/tail, and does not support multi-channel privmsg
prefixMessage _ _            = False

whichChannel :: BotEvent -> BS.ByteString
whichChannel (EMessage m) = head $ IRC.msg_params m -- TODO: unsafe head, and does not support multi-channel privmsg
whichChannel _            = ""
