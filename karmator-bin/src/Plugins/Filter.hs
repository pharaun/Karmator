{-# LANGUAGE OverloadedStrings #-}
module Plugins.Filter
    ( whichChannel
    , messageContent
    , nickContent
    , userNameContent
    , hostMaskContent
    ) where

import Safe
import qualified Data.ByteString as BS
import Karmator.Types

-- TODO: migrate to karmator-irc
import qualified Network.IRC as IRC


-- TODO: implement a message Content typeclass


-- TODO: does not support multi-channel privmsg
whichChannel :: BotEvent IRC.Message -> BS.ByteString
whichChannel (EMessage m) = headDef "" $ IRC.msg_params m
whichChannel _              = ""

-- TODO: does not support multi-channel privmsg
messageContent :: BotEvent IRC.Message -> BS.ByteString
messageContent (EMessage m) = headDef "" $ tailSafe $ IRC.msg_params m
messageContent _              = ""

nickContent :: BotEvent IRC.Message -> BS.ByteString
nickContent (EMessage (IRC.Message{IRC.msg_prefix=(Just (IRC.NickName n _ _))})) = n
nickContent _                                                                      = ""

userNameContent :: BotEvent IRC.Message -> Maybe BS.ByteString
userNameContent (EMessage (IRC.Message{IRC.msg_prefix=(Just (IRC.NickName _ u _))})) = u
userNameContent _                                                                      = Nothing

hostMaskContent :: BotEvent IRC.Message -> Maybe BS.ByteString
hostMaskContent (EMessage (IRC.Message{IRC.msg_prefix=(Just (IRC.NickName _ _ m))})) = m
hostMaskContent _                                                                      = Nothing
