{-# LANGUAGE OverloadedStrings #-}
module Karmator.Bot
    ( runBot
    , executeCmdRef

    , sqlWrapper -- TODO: pull into the cmd exec bits
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.List as DL
import Prelude hiding (log)
import Control.Concurrent (forkIO, threadDelay)

-- Karmator Stuff
import Karmator.Route
import Karmator.Server
import Karmator.Types

-- Temp?
import Database.Persist.Sql (ConnectionPool)

-- | Same as '>>', but with the arguments interchanged.
k << m      = (\_ -> k) =<< m
{-# INLINE (<<) #-}


runBot :: [ServerConfig] -> Route [CmdHandler] -> IO ()
runBot s r = do
    -- Start up the bot command + route
    q <- newTQueueIO
    bot <- async (runCommand q [r])

    -- Give the input to each server thread and spawn them
    servers <- mapM (\sc -> async (runServer sc q)) s

    -- TODO: more sophsicated logic here, we exit upon shutdown of any
    -- server/bot async
    _ <- waitAnyCancel (bot : servers)
    return ()

runCommand :: TQueue (BotEvent, TQueue BotCommand) -> [Route [CmdHandler]] -> IO ()
runCommand q routes = forever $ do
    (msg, reply) <- atomically $ readTQueue q

    cmdRefs <- runRoute (choice routes) msg
    results <- executeCmdRef cmdRefs msg

    -- TODO: filter out DMessage and deal with them specifically
    let (deferSend, nowSend) = DL.partition delayMsg $ DL.concat results
    mapM_ (atomically . writeTQueue reply) nowSend
    mapM_ (sendDelayMsg reply) deferSend

  where
    delayMsg (DMessage _ _) = True
    delayMsg _              = False

    -- TODO: we probably want a nicer way of doing this but just creating
    -- a bunch of thread and sleeping then sending seems adequate for now
    sendDelayMsg reply (DMessage t msg) = return () << (forkIO $ do
        threadDelay (t * 1000000)
        atomically . writeTQueue reply $ CMessage msg)
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
