{-# LANGUAGE OverloadedStrings #-}
module Karmator.Server.Slack
    ( runServer
    , SlackConfig
    , getServerConfig
    ) where

import Safe
import System.IO
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Error
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict hiding (get)
import Data.Typeable
import Prelude hiding (log, head, tail)
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as BS

--
-- TODO: For config bits
--
import Data.ConfigFile
import Data.Set (Set)
import Data.Monoid ((<>))
import qualified Data.Set as Set
--
-- Config bits above
--

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
    { network :: String
    , apiToken :: String
    }
    deriving (Show)

--
-- Establish and run a server connection (tls/plain)
--
runServer :: ServerConfig SlackConfig -> TQueue (BotEvent, TQueue BotCommand) -> IO ()
runServer sc queue = undefined

--
-- Config
--
getServerConfig
    :: ConfigParser
    -> String
    -> ExceptT CPError IO (ServerConfig SlackConfig, (String, [BS.ByteString], Set BS.ByteString, Int, [BS.ByteString]))
getServerConfig c s = do
    apitoken  <- get c s "api_token"
    logfile   <- get c s "logfile"
    logslack  <- get c s "logslack"
    reconn    <- get c s "reconn"
    reWait    <- get c s "reconn_wait" -- In seconds

    let config = SlackConfig s apitoken
    return (ServerConfig config reconn (reWait * 1000000) logfile logslack, (s, [], Set.fromList [], 0, []))
