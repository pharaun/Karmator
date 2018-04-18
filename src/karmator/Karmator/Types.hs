{-# LANGUAGE OverloadedStrings, ExistentialQuantification, DeriveFunctor, FlexibleInstances #-}
module Karmator.Types
    ( ServerConfig(..)
    , ServerState(..)

    -- Route
    , Route
    , CmdHandler

    -- Events
    , ServerEvent(..)
    , BotEvent(..)
    , BotCommand(..)

    -- Internal
    , Segment(..)
    , CmdRef(..)

    -- External Handler
    , ExternalHandler(..)
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

-- Server Connect Loop Events
--  1. Connection lost
--  2. Termination
data ServerEvent
    = RecvLost
    | SendLost
    deriving (Show)

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
--  4. Delayed Message (x second delay) (Supports Throttled Messages)
data BotCommand
    = Disconnect
    | Die
    | CMessage IRC.Message
    | DMessage Int IRC.Message
    | Register ExternalHandler
    deriving (Show)

instance Eq BotCommand where
    Disconnect == Disconnect        = True
    Die == Die                      = True
    CMessage x == CMessage y        = x == y
    DMessage x x' == DMessage y y'  = (x == y) && (x' == y')
    Register _ == Register _        = False
    _ == _                          = False

-- Routing
data Segment m p i o n
    = Match (i -> Bool) n
    | Choice [n]
    | Handler (CmdRef m p i o)
    deriving (Functor, Show)

type RouteT m p a = FreeT (Segment m p BotEvent [BotCommand]) m a
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

type CmdHandler = CmdRef IO ConnectionPool BotEvent [BotCommand]


-- TODO: Implement ExternalHandler
--  External Handler is a snippet of code that works via external stimulus
--      (Network calls, timers, etc...), so an list is a list of monadic
--      compution to be ran, I think ultimately in the end what it will be
--      is a [list] of live external threads, that when they die, they
--      inform the bot on if they want to be relaunched or not. Then there
--      should be a way to register additional external handler or not
--
--      CmdHandler can emit an new "message" for Init+registering a new
--      external handler
--
--      Alternativly an external handler can be started up at bot start
--      time via a init list.
--
--      Need to identify how to handle per server stuff or what (basically
--      sending data out)
--
--      Basically, upon bot start/connection established, we emit an
--      "connection established" and then each plugin route can listen to
--      this and then register their external handler.  Biggest question is
--      how to handle connection loss/disconnect, probably give that
--      message to the external handler and let them decide if they want to
--      shut down or not.
--
--  TODO:
--      1. A way to assign an external handler to a particular network connection
--      2. A way to either only launch an EH once and have it handle reconn or a way to kill + relaunch
--      3. A way to share state in the EH with the rest of the plugin + possibly give access to persistent data
--      4. if a EH cares about connection lost+establish, the plugin can pass that info in
--      5. Relaunch if the EH crashes?
--
--  Name, Network/any, (EH Queue)
data ExternalHandler = ExtRef String [String] (TQueue BotCommand -> IO ()) -- TODO: io for now
-- Open questions:
--  - ExtRef should be able to register more extref to be launched
--  - DMessages should get put into the delay thing like other messages
--  - Probably needs a dedicated EH queue (so that we can handle the
--      register, and sending it to the right network)
--  - Implement a Network type for mapping Network. 'Network String | All'
--      * Where String is the network name in the config (then we need to have mapping from the network to the TQueue)
--      * Catchall 'All' for broadcasting to all network

-- data NetworkType = Network String | All
-- type NetworkQueue = Map.Map NetworkType (TQueue BotCommand)

instance Show ExternalHandler where
    show (ExtRef n _ _)       = "External Handler: " ++ show n
