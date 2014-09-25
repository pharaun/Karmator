{-# LANGUAGE OverloadedStrings, ExistentialQuantification, DeriveFunctor, FlexibleInstances #-}
module Karmator.Types
    ( ServerConfig(..)
    , ServerPersistentState(..)
    , ServerState(..)

    , BotConfig(..)
    , BotState(..)

    -- TODO: not sure this is best spot
    , CmdRef(..)
    , CmdHandler(..)
    , Route(..)
    , Segment(..)
    ) where

import Network
import System.IO
import System.Time
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Text.Show.Functions

import qualified Data.ByteString as BS
import qualified Network.IRC as IRC

-- Bot configuration
data BotConfig = BotConfig {}
data BotState m = BotState
    { botConfig :: BotConfig
    , serverQueue :: TQueue (IRC.Message, TQueue IRC.Message)

    , servers :: [(Bool, ServerConfig, ServerPersistentState)]
    , routes :: [Route m [CmdHandler m]]

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
    }

-- TODO: Replace bare state with (MVar state) or something, maybe even a hook to the plugin's module level stuff
--data CommandRef m = forall st. CommandRef (Command m st) (MVar st)
data CmdRef m i o = forall st. CmdRef String st (st -> i -> m o)
instance Show (CmdRef m i o) where
    show (CmdRef n _ _) = "Command: " ++ show n
type CmdHandler m = CmdRef m IRC.Message (Maybe IRC.Message)


-- ROUTE STUFF
data Segment m i o n
    = Match (i -> Bool) n
    | Choice [n]
    | Handler (CmdRef m i o)
    deriving (Functor, Show)

-- | Newtype of the freeT transformer stack
type Route m a = FreeT (Segment m IRC.Message (Maybe IRC.Message)) m a
--newtype Route m a = FreeT (Segment IRC.Message (Maybe IRC.Message)) m a
