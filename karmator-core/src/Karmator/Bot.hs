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

-- Temp?
import Database.Persist.Sql (ConnectionPool)


--
-- Just preload the config into the runner and feed it here
-- TODO: instead of only a route, have a map of route (String -> Route),
-- and a default Route. So that we can match on network and then the
-- default route
-- TODO: alternativly figure out a way to wire and/or route messages from
-- the correct server/network to the correct bot thread, ie have one thread
-- bot executor for each network + one for the default, and dispatch the
-- network messages to the correct one as needed
-- TODO: maybe have a thing where you have (Network, Server, NetworkRouter)
-- plus defaultRouter and then the function will init the server, wire it
-- up to the network router + somehow wire it also into the default router
-- TODO: need a way to launch separate typed message to support irc and
-- slack....
--
runBot
    :: [TQueue (String, BotEvent a, TQueue (BotCommand a)) -> IO ()]
    -> Route [CmdHandler a] a
    -> IO ()
runBot serverRunner botRoute = do

    -- TODO: register a custom runEH command here for handling EH output
    -- and routing to the right network
    -- q' <- newTQueueIO
    -- runEH q'

    -- Start up the bot command + route
    q <- newTQueueIO
    bot <- async (runCommand q [botRoute]) -- add q' here

    -- TODO: construct a mapping between network & TQueue
    -- Give the input to each server thread and spawn them
    servers <- mapM (\sr -> async $ sr q) serverRunner
    --ircServers   <- mapM (\sc -> async (IRC.runServer sc q)) sic
    --slackServers <- mapM (\sc -> async (Slack.runServer sc q)) ssc

    -- TODO: more sophsicated logic here, we exit upon shutdown of any
    -- server/bot async
    _ <- waitAnyCancel (bot : servers)
    --_ <- waitAnyCancel (bot : (ircServers ++ slackServers))
    return ()


runCommand :: TQueue (String, BotEvent a, TQueue (BotCommand a)) -> [Route [CmdHandler a] a] -> IO ()
runCommand q routes = forever $ do
    -- TODO: expose the tag to something so we can act upon it
    (networkTag, msg, reply) <- atomically $ readTQueue q

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
executeCmdRef :: [CmdHandler a] -> BotEvent a -> IO [[BotCommand a]]
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
sqlWrapper :: MonadIO m => (BotEvent a -> ReaderT ConnectionPool m [BotCommand a]) -> ConnectionPool -> BotEvent a -> m [BotCommand a]
sqlWrapper c pool e = runReaderT (c e) pool
