{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    ( runBot
    , executeCmdRef

    , sqlWrapper -- TODO: pull into the cmd exec bits
    ) where

import Control.Monad (void)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.List as DL
import Prelude hiding (log)
import Control.Concurrent (forkIO, threadDelay)

-- Karmator Stuff
import Karmator.Route
import Karmator.Types

import qualified Karmator.Server.IRC as IRC
import qualified Karmator.Server.Slack as Slack

-- Temp?
import Database.Persist.Sql (ConnectionPool)

runBot :: [ServerConfig IRC.IrcConfig] -> [ServerConfig Slack.SlackConfig] -> Route [CmdHandler] -> IO ()
runBot sic ssc r = do
    -- TODO: register a custom runEH command here for handling EH output
    -- and routing to the right network
    -- q' <- newTQueueIO
    -- runEH q'

    -- Start up the bot command + route
    q <- newTQueueIO
    bot <- async (runCommand q [r]) -- add q' here

    -- TODO: construct a mapping between network & TQueue
    -- Give the input to each server thread and spawn them
    ircServers   <- mapM (\sc -> async (IRC.runServer sc q)) sic
    slackServers <- mapM (\sc -> async (Slack.runServer sc q)) ssc

    -- TODO: more sophsicated logic here, we exit upon shutdown of any
    -- server/bot async
    _ <- waitAnyCancel (bot : (ircServers ++ slackServers))
    return ()


runCommand :: TQueue (BotEvent, TQueue BotCommand) -> [Route [CmdHandler]] -> IO ()
runCommand q routes = forever $ do
    (msg, reply) <- atomically $ readTQueue q

    cmdRefs <- runRoute (choice routes) msg
    results <- executeCmdRef cmdRefs msg

    -- TODO: Register ExternalHandler (with the given bot queue) (from plugin/route)

    -- TODO: filter out DMessage and deal with them specifically
    let (deferSend, nowSend) = DL.partition delayMsg $ DL.concat results
    mapM_ (atomically . writeTQueue reply) nowSend
    mapM_ (sendDelayMsg reply) deferSend

  where
    delayMsg (DMessage _ _) = True
    delayMsg _              = False

    -- TODO: we probably want a nicer way of doing this but just creating
    -- a bunch of thread and sleeping then sending seems adequate for now
    sendDelayMsg reply (DMessage t msg) = void $ forkIO $ do
        threadDelay (t * 1000000)
        atomically . writeTQueue reply $ CMessage msg
    sendDelayMsg _ _ = return ()


--
-- Execute the list of commands
--
-- TODO: Improve error handling here. CmdHandler can crash (its running
-- external code), so should be able to capture these:
-- https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
--
-- Also should eventually parallelize and other improvement on this region.
--
executeCmdRef :: [CmdHandler] -> BotEvent -> IO [[BotCommand]]
executeCmdRef cs m = mapM (mapCmdRef m) cs
  where
    mapCmdRef m' (CmdRef _ h)        = h m'
    mapCmdRef m' (SCmdRef _ st h)    = h st m'
    mapCmdRef m' (PCmdRef _ p h)     = h p m'
    mapCmdRef m' (PSCmdRef _ p st h) = h p st m'

---- Handler Type
---- TODO: replace st with (MVar st)
---- st - Ephemeral State
---- p - Persistent State (db connection/etc)
--data CmdRef m p i o = CmdRef String (i -> m o)
--                    | forall st . SCmdRef String st (st -> i -> m o)
--                    | PCmdRef String p (p -> i -> m o)
--                    | forall st . PSCmdRef String p st (p -> st -> i -> m o)
--
--instance Show (CmdRef m p i o) where
--    show (CmdRef n _)       = "Pure Command: " ++ show n
--    show (SCmdRef n _ _)    = "Stateful Command: " ++ show n
--    show (PCmdRef n _ _)    = "Persistance Command: " ++ show n
--    show (PSCmdRef n _ _ _) = "Persistance Stateful Command: " ++ show n


-- inviteJoin :: MonadIO m => BotEvent -> ReaderT ConnectionPool m [BotCommand]

--
-- Test the wrapper stuff so i don't need to modify all over
-- TODO: move the pool management stuff to the cmdexecute stuff
--
sqlWrapper :: MonadIO m => (BotEvent -> ReaderT ConnectionPool m [BotCommand]) -> ConnectionPool -> BotEvent -> m [BotCommand]
sqlWrapper c pool e = runReaderT (c e) pool
