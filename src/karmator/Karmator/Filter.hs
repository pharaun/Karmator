{-# LANGUAGE OverloadedStrings #-}
module Karmator.Filter
    ( exactCommand
    , prefixMessage
    , commandMessage
    , networkMatch
    , whichChannel
    , messageContent
    , nickContent
    , userNameContent
    , hostMaskContent
    ) where

import Safe
import qualified Data.ByteString as BS
import qualified Network.IRC as IRC
import Karmator.Types


--
-- Simple Filters to start of, flat-matches
--

exactCommand :: BS.ByteString -> BotEvent -> Bool
exactCommand c (EMessage _ m) = c == IRC.msg_command m
exactCommand _ _              = False

-- TODO: does not support multi-channel privmsg
prefixMessage :: BS.ByteString -> BotEvent -> Bool
prefixMessage c (EMessage _ m) = c `BS.isPrefixOf` (BS.dropWhile (space ==) $ headDef "" (tailSafe $ IRC.msg_params m))
  where
    space = BS.head " "
prefixMessage _ _              = False

-- TODO: does not support multi-channel privmsg
-- TODO: add in a config prefix for defining a command
commandMessage :: BS.ByteString -> BotEvent -> Bool
commandMessage c (EMessage _ m) = c == (BS.takeWhile (space /=) $ BS.dropWhile (space ==) $ headDef "" (tailSafe $ IRC.msg_params m))
  where
    space = BS.head " "
commandMessage _ _              = False

networkMatch :: String -> BotEvent -> Bool
networkMatch n (EMessage n' _) = n == n'
networkMatch _ _               = False

-- TODO: does not support multi-channel privmsg
whichChannel :: BotEvent -> BS.ByteString
whichChannel (EMessage _ m) = headDef "" $ IRC.msg_params m
whichChannel _              = ""

-- TODO: does not support multi-channel privmsg
messageContent :: BotEvent -> BS.ByteString
messageContent (EMessage _ m) = headDef "" $ tailSafe $ IRC.msg_params m
messageContent _              = ""

nickContent :: BotEvent -> BS.ByteString
nickContent (EMessage _ (IRC.Message{IRC.msg_prefix=(Just (IRC.NickName n _ _))})) = n
nickContent _                                                                      = ""

userNameContent :: BotEvent -> Maybe BS.ByteString
userNameContent (EMessage _ (IRC.Message{IRC.msg_prefix=(Just (IRC.NickName _ u _))})) = u
userNameContent _                                                                      = Nothing

hostMaskContent :: BotEvent -> Maybe BS.ByteString
hostMaskContent (EMessage _ (IRC.Message{IRC.msg_prefix=(Just (IRC.NickName _ _ m))})) = m
hostMaskContent _                                                                      = Nothing
