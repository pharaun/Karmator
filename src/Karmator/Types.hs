{-# LANGUAGE OverloadedStrings, ExistentialQuantification, DeriveFunctor, FlexibleInstances #-}
module Karmator.Types
    ( ServerConfig(..)
    , ServerState(..)

    -- Route
    , Route
    , CmdHandler

    -- Events
    , BotEvent(..)
    , BotCommand(..)

    -- Internal
    , Segment(..)
    , CmdRef(..)
    ) where

import Network
import System.IO
import Control.Concurrent.STM
import Control.Monad.Trans.Free
import Text.Show.Functions()


import qualified Data.ByteString as BS
import qualified Network.IRC as IRC

-- Per server config for the bot
data ServerConfig = ServerConfig
    { server :: String
    , port :: PortNumber
    , nicks :: [BS.ByteString] -- First one then alternatives in descending order
    , userName :: BS.ByteString
    , serverPassword :: Maybe BS.ByteString

    -- TODO
    , reconnect :: Bool

    -- Default set of channels to always join
    , channels :: [BS.ByteString]

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

-- Ephemeral State:
data ServerState = ServerState
    { config :: ServerConfig
    , logStream :: Handle

    , botQueue :: TQueue (BotEvent, TQueue BotCommand)
    , replyQueue :: TQueue BotCommand
    }

-- Bot/Server Events
--  0. Bot init
--  1. Connection established
--  2. connection lost
--  3. Normal IRC message
data BotEvent
    = BotInit
    | ConnectionEstablished
    | ConnectionLost
    | EMessage IRC.Message
    deriving (Show)

-- Bot/Server Commands
--  1. Disconnect - if sent to a server, disconnect
--  2. Die - If emitted the bot needs to die
--  3. Message
data BotCommand
    = Disconnect
    | Die
    | CMessage IRC.Message
    deriving (Show)

-- Routing
data Segment m i o n
    = Match (i -> Bool) n
    | Choice [n]
    | Handler (CmdRef m i o)
    deriving (Functor, Show)

type RouteT m a = FreeT (Segment m BotEvent (Maybe BotCommand)) m a
type Route a = RouteT IO a


-- Handler Type
-- TODO: replace st with (MVar st)
data CmdRef m i o = forall st. CmdRef String st (st -> i -> m o)

instance Show (CmdRef m i o) where
    show (CmdRef n _ _) = "Command: " ++ show n

type CmdHandler = CmdRef IO BotEvent (Maybe BotCommand)
