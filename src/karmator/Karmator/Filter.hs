{-# LANGUAGE OverloadedStrings #-}
module Karmator.Filter
    ( exactCommand
    , prefixMessage
    , whichChannel
    , messageContent
    , nickContent
    ) where

import Safe
import qualified Data.ByteString as BS
import qualified Network.IRC as IRC
import Karmator.Types


--
-- Simple Filters to start of, flat-matches
--

exactCommand :: BS.ByteString -> BotEvent -> Bool
exactCommand c (EMessage m) = c == IRC.msg_command m
exactCommand _ _            = False

-- TODO: does not support multi-channel privmsg
prefixMessage :: BS.ByteString -> BotEvent -> Bool
prefixMessage c (EMessage m) = c `BS.isPrefixOf` headDef "" (tailSafe $ IRC.msg_params m)
prefixMessage _ _            = False

-- TODO: does not support multi-channel privmsg
whichChannel :: BotEvent -> BS.ByteString
whichChannel (EMessage m) = headDef "" $ IRC.msg_params m
whichChannel _            = ""

-- TODO: does not support multi-channel privmsg
messageContent :: BotEvent -> BS.ByteString
messageContent (EMessage m) = headDef "" $ tailSafe $ IRC.msg_params m
messageContent _            = ""

nickContent :: BotEvent -> BS.ByteString
nickContent (EMessage (IRC.Message{IRC.msg_prefix=(Just (IRC.NickName n _ _))})) = n
nickContent _                                                                    = ""
