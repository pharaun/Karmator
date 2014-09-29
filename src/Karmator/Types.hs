{-# LANGUAGE OverloadedStrings #-}
module Karmator.Types
    ( ServerConfig(..)
    , ServerPersistentState(..)
    , ServerState(..)

    , BotConfig(..)
    , BotState(..)
    ) where

import Network
import System.IO
import System.Time
import Control.Concurrent.STM
import Control.Monad.Trans.Free

import qualified Data.ByteString as BS
import qualified Network.IRC as IRC

-- Karmator
import Karmator.Route

-- Bot configuration
data BotConfig = BotConfig {}
data BotState = BotState
    { botConfig :: BotConfig
    , serverQueue :: TQueue (IRC.Message, TQueue IRC.Message)

    , servers :: [(Bool, ServerConfig, ServerPersistentState)]
    , routes :: [Route [CmdHandler]]

    -- Debugging/initial test impl
    , startTime :: ClockTime
    }

-- Per server config for the bot
data ServerConfig = ServerConfig
    { server :: String
    , port :: PortNumber
    , nicks :: [BS.ByteString] -- First one then alternatives in descending order
    , userName :: BS.ByteString
    , serverPassword :: Maybe BS.ByteString

    -- TODO
    , reconnect :: Bool

    , logfile :: String -- logfile

    -- Function for default encoding and decoding
--    , defaultEncoding :: BS.ByteString -> T.Text
--    , defaultDecoding :: T.Text -> BS.ByteString

    -- Rates:
    --  messages_per_seconds, server_queue_size
    --
    -- Messages:
    --  message_split_start, message_split_end, max_messages, encoding
    --
    -- Timeouts:
    --  read - 240s
    --  connect - 10s
    --  ping_interval
    --  max_reconnect_delay
    --  delay_joins
    --
    -- Security:
    --  ssl {use, verify, client_cert, ca_path}
    --  sasl {username, password}
    --  serverauth {username, password}
    --
    -- Misc Server:
    --  log-level
    --  realname
    --  modes
    --
    }


-- Persistent State: TODO: probably can just fold into server config
data ServerPersistentState = ServerPersistentState
    { channels :: [BS.ByteString]
--   channels, encoding
    }

-- Ephemeral State:
data ServerState = ServerState
    { session :: ServerPersistentState
    , config :: ServerConfig
    , logStream :: Handle

    , botQueue :: TQueue (IRC.Message, TQueue IRC.Message)
    , replyQueue :: TQueue IRC.Message
    }
