{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Karmator.Types
    ( ServerConfig(..)
    , ServerPersistentState(..)
    , ServerState(..)

    -- TODO: not sure this is best spot
    , CmdRef(..)
    ) where

import Network
import System.IO
import System.Time

import qualified Data.ByteString as BS

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


-- Persistent State:
data ServerPersistentState = ServerPersistentState
    { channels :: [BS.ByteString]
--   channels, encoding
    }

-- Ephemeral State:
data ServerState = ServerState
    { session :: ServerPersistentState
    , config :: ServerConfig
    , logStream :: Handle

    -- Debugging/initial test impl
    , startTime :: ClockTime
    }

-- TODO: Replace bare state with (MVar state) or something, maybe even a hook to the plugin's module level stuff
--data CommandRef m = forall st. CommandRef (Command m st) (MVar st)
data CmdRef i o = forall st. CmdRef String st (st -> i -> o)
instance Show (CmdRef i o) where
    show (CmdRef n _ _) = "Command: " ++ show n
