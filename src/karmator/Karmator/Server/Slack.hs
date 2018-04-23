{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server.Slack
    ( runServer
    , SlackConfig(..)
    ) where

import Safe
import System.IO
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Data.Typeable
import Prelude hiding (log, head, tail)
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS

-- TODO: kill this here
import qualified Data.ByteString.Char8 as C8

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PP

-- Slack Parser


-- Karmator Stuff
import Karmator.Types


--
-- Server Specific configs
--
data SlackConfig = SlackConfig
    { apiToken :: String
    }
    deriving (Show)

--
-- Establish and run a server connection (tls/plain)
--
runServer :: ServerConfig SlackConfig -> TQueue (BotEvent, TQueue BotCommand) -> IO ()
runServer sc queue = undefined
