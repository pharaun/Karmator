{-# LANGUAGE OverloadedStrings, ExistentialQuantification, DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module Karmator.Types
    ( ServerConfig(..)
    , ServerState(..)

    -- Route
    , Route
    , CmdHandler

    -- Events
    , ServerEvent(..)
    , BotEvent(..)
    , BotEventMatch(..)
    , BotCommand(..)

    -- Internal
    , Segment(..)
    , CmdRef(..)

    -- External Handler
    , ExternalHandler(..)
    ) where

import System.IO
import Control.Concurrent.STM
import Control.Monad.Trans.Free
import Text.Show.Functions()
import Database.Persist.Sql (ConnectionPool)


-- Per server config for the bot
data ServerConfig a = ServerConfig
    { serverSpecific :: a
    , networkTag :: String
    , reconnect :: Bool
    , reconnectWait :: Int -- Microseconds

    -- TODO: make this optional/replaced by a logging infrastructure
    , logfile :: String
    , logMsg :: Bool

    -- Rates:
    --  messages_per_seconds, server_queue_size
    --
    -- Messages:
    --  message_split_start, message_split_end, max_messages, encoding
    --  modes
    }
    deriving (Show)


-- Ephemeral State:
data ServerState a b c = ServerState
    { config :: ServerConfig a
    , logStream :: Handle

    , botQueue :: TQueue (String, BotEvent b, TQueue (BotCommand b))
    , replyQueue :: TQueue (BotCommand b)

    , connectionSuccess :: TVar Bool

    , botState :: TVar c
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
--  3. Normal x message
--
--  TODO: (Useful for plugin system maybe)
--   1. Bot init
--   2. Bot shutdown
--   3. Auth completed (failed?)
data BotEvent a
    = ConnectionEstablished
    | ConnectionLost
    | EMessage a
    deriving (Show)

--
-- Event Match for common matchers
--
class BotEventMatch a b where
    exactCommand   :: a -> BotEvent b -> Bool
    -- TODO: does not support multi-channel privmsg
    prefixMessage  :: a -> BotEvent b -> Bool
    -- TODO: does not support multi-channel privmsg
    -- TODO: add in a config prefix for defining a command
    commandMessage :: a -> BotEvent b -> Bool
    nickMatch      :: a -> BotEvent b -> Bool

-- Bot/Server Commands
--  1. Disconnect - if sent to a server, disconnect
--  2. Die - If emitted the bot needs to die
--  3. Message
--  4. Delayed Message (x second delay) (Supports Throttled Messages)
data BotCommand a
    = Disconnect
    | Die
    | CMessage a
    | DMessage Int a
    | Register (ExternalHandler a)
    deriving (Show)

instance (Eq a) => Eq (BotCommand a) where
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

type RouteT m p a b = FreeT (Segment m p (BotEvent b) [BotCommand b]) m a
-- TODO: maybe neat to add in support for varying persistance method, but for now force one
type Route a b = RouteT IO ConnectionPool a b


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

type CmdHandler a = CmdRef IO ConnectionPool (BotEvent a) [BotCommand a]


-- TODO: Delayed messages and external handler, could in theory be a new serverRunner,
--      The main unanswered/unknown question is how to find and map to the relevant/right network and
--      cross network/plugin/server runner sharing of some data...

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
data ExternalHandler a = ExtRef String [String] (TQueue (BotCommand a) -> IO ()) -- TODO: io for now
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

instance Show (ExternalHandler a) where
    show (ExtRef n _ _)       = "External Handler: " ++ show n
