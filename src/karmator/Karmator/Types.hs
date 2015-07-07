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
import Database.Persist.Sql (ConnectionPool)

import qualified Data.ByteString as BS
import qualified Network.IRC as IRC
import qualified Network.TLS as TLS


-- Per server config for the bot
data ServerConfig = ServerConfig
    { network :: String
    , server :: String
    , port :: PortNumber
    , nicks :: [BS.ByteString] -- First one then alternatives in descending order
    , userName :: BS.ByteString
    , serverPassword :: Maybe BS.ByteString

    , tlsSettings :: Maybe TLS.ClientParams

    , reconnect :: Bool
    , reconnectWait :: Int -- Microseconds

    -- TODO: make this optional/replaced by a logging infrastructure
    , logfile :: String
    , logIrc :: Bool

    -- Function for default encoding and decoding
    -- defaultEncoding :: BS.ByteString -> T.Text
    -- defaultDecoding :: T.Text -> BS.ByteString
    --
    -- Rates:
    --  messages_per_seconds, server_queue_size
    --
    -- Messages:
    --  message_split_start, message_split_end, max_messages, encoding
    --  modes
    --
    -- Timeouts:
    --  read - 240s
    --  connect - 10s
    --  ping_interval
    --  max_reconnect_delay
    --  delay_joins
    --
    -- Security:
    --  sasl {username, password}
    --  serverauth {username, password}
    }
    deriving (Show)

-- Ephemeral State:
data ServerState = ServerState
    { config :: ServerConfig
    , logStream :: Handle

    , botQueue :: TQueue (BotEvent, TQueue BotCommand)
    , replyQueue :: TQueue BotCommand

    , connectionSuccess :: TVar Bool
    }

-- Bot/Server Events
--  1. Connection established
--  2. Connection lost
--  3. Normal IRC message
--
--  TODO: (Useful for plugin system maybe)
--   1. Bot init
--   2. Bot shutdown
--   3. Auth completed (failed?)
data BotEvent
    = ConnectionEstablished
    | ConnectionLost
    | EMessage String IRC.Message
    deriving (Show)

-- Bot/Server Commands
--  1. Disconnect - if sent to a server, disconnect
--  2. Die - If emitted the bot needs to die
--  3. Message
data BotCommand
    = Disconnect
    | Die
    | CMessage IRC.Message
    deriving (Show,Eq)

-- Routing
data Segment m p i o n
    = Match (i -> Bool) n
    | Choice [n]
    | Handler (CmdRef m p i o)
    deriving (Functor, Show)

type RouteT m p a = FreeT (Segment m p BotEvent (Maybe BotCommand)) m a
-- TODO: maybe neat to add in support for varying persistance method, but for now force one
type Route a = RouteT IO ConnectionPool a


-- Handler Type
-- TODO: replace st with (MVar st)
-- st - Ephemeral State
-- p - Persistent State (db connection/etc)
data CmdRef m p i o = CmdRef String (i -> m o)
                    | forall st . SCmdRef String st (st -> i -> m o)
                    | PCmdRef String p (p -> i -> m o)
                    | forall st . PSCmdRef String p st (p -> st -> i -> m o)

instance Show (CmdRef m p i o) where
    show (CmdRef n _)       = "Pure Command: " ++ show n
    show (SCmdRef n _ _)    = "Stateful Command: " ++ show n
    show (PCmdRef n _ _)    = "Persistance Command: " ++ show n
    show (PSCmdRef n _ _ _) = "Persistance Stateful Command: " ++ show n

type CmdHandler = CmdRef IO ConnectionPool BotEvent (Maybe BotCommand)
