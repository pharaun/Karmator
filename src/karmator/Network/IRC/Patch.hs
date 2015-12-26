{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Patch
    ( optionMaybe
    , tokenize
    , message
    , mkMessage
    , pass

    ) where

import qualified Data.ByteString as BS

-- IRC Parser
import qualified Network.IRC as IRC
import Control.Applicative
import Data.Attoparsec.ByteString


-- UPSTREAM this to Network.IRC.Parser
--------------------------------------------------------------------------
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

tokenize  :: Parser a -> Parser a
tokenize p = p >>= \x -> IRC.spaces >> return x

-- Streaming version of the IRC message parser
message :: Parser IRC.Message
message  = do
    p <- optionMaybe $ tokenize IRC.prefix
    c <- IRC.command
    ps <- many (IRC.spaces >> IRC.parameter)
    _ <- IRC.crlf
    return $ IRC.Message p c ps


mkMessage :: BS.ByteString -> [IRC.Parameter] -> IRC.Message
mkMessage = IRC.Message Nothing

pass  :: IRC.Password -> IRC.Message
pass u = mkMessage "PASS" [u]
--------------------------------------------------------------------------
