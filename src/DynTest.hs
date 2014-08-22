{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses #-}

import Data.List hiding (init)
import System.Time
import Control.Monad
import Control.Monad.IO.Class
import Prelude hiding (log, init)

import Data.Maybe
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Reader

import qualified Data.ByteString as B

import Control.Concurrent.MVar


-- TEMPLATE HASKELL
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax hiding (lift)

moduleNameTH :: Quasi m => m Exp
moduleNameTH = fmap loc_module qLocation >>= \mods -> return (AppE (VarE (mkName "Data.ByteString.Char8.pack")) (LitE (StringL mods)))
-- TEMPLATE HASKELL

type Msg = String


--
-- Final version that trys to incorporate all of the nice features of all
-- of the other versions
--
data Command m st = Command
    { commandName  :: String
    , commandHelp  :: [Msg] -- TODO: not sure how to integrate commandHelp, but for now if "help <command> matches the command name, it'll invoke the help"
    , commandMatch :: Msg -> ReaderT st m Bool
    , commandEval  :: Msg -> StateT st m (Maybe [Msg])
    }

instance Show (Command m st) where
    show (Command n _ _ _) = "Cmd: " ++ n

-- Deals in serialization of the state for the plugin
-- TODO: customize it so that I can provide some default serialization such as json, etc
data Serial st = Serial
    { serialize   :: st -> Maybe B.ByteString
    , deserialize :: B.ByteString -> Maybe st
    }

-- Module that contains the plugins
data Module m st = Module
    -- TODO: this should be handled by some template haskell maybe
    { moduleName :: String
    , moduleSerialize :: m (Maybe (Serial st))

    -- TODO: may want to better separate default value versus init/exit
    , moduleInit :: m st
    , moduleExit :: m ()

    , moduleCmds :: [Command m st]
    }

instance Show (Module m st) where
    show (Module n _ _ _ c) = "Mod: " ++ n ++ " Cmds: " ++ show c



-- This data structure stuff is purely for the ircbot to keep track of the
-- modules and commands
data ModuleRef m = forall st. ModuleRef (Module m st) (MVar st)
data CommandRef m = forall st. CommandRef (Command m st) (MVar st)

instance Show (ModuleRef m) where
    show (ModuleRef m _) = "Module: " ++ show m ++ " state: -"

instance Show (CommandRef m) where
    show (CommandRef c _) = "Command: " ++ show c ++ " state: -"

--
-- This registers a module defined above into a format suitable for
-- processing by the ircbot in a generic manner
--
loadModule :: (Monad m, MonadIO m) => Module m st -> m (ModuleRef m, [CommandRef m])
loadModule m = do
    -- TODO: support for deserialize here, or take default state then init module
    state' <- moduleInit m
    ref <- liftIO $ newMVar state'
    let cmdref = map (`CommandRef` ref) (moduleCmds m)

    return (ModuleRef m ref, cmdref)

-- TODO: figure out how to remove commands or only use this strictly for shutdown
unloadModule :: (Monad m, MonadIO m) => ModuleRef m -> m ()
unloadModule _ = undefined


helpCommands :: (Monad m, MonadIO m) => Msg -> [CommandRef m] -> m (Maybe [Msg])
helpCommands m cs
    -- TODO: replace with a real match
    | "help" `isPrefixOf` m = do
        let match = filter (\(CommandRef (Command n _ _ _) _) -> n `isSuffixOf` m) cs
        case match of
            (CommandRef (Command _ h _ _) _):_ -> return $ Just h
            []    -> return Nothing
    | otherwise             = return Nothing


-- TODO: look into early termination to avoid effectful computation/updates of states if we can avoid it,
--      we may be safe here because of thunks but let's verify esp with modifyMVar involved
--runCommands :: (Monad m, MonadIO m) => Msg -> [CommandRef m] -> m (Maybe [Msg])
runCommands :: Msg -> [CommandRef IO] -> IO (Maybe [Msg])
runCommands m cs = listToMaybe <$> catMaybes <$> mapM (runCommand m) cs


-- TODO: find out why the typeclass monad/monadio isn't working right
--runCommand :: (Monad m, MonadIO m) => Msg -> CommandRef m -> m (Maybe [Msg])
runCommand :: Msg -> CommandRef IO -> IO (Maybe [Msg])
runCommand m (CommandRef c st) = liftIO $ modifyMVar st (\s -> do
    matched <- liftIO $ runReaderT (c `commandMatch` m) s
    if matched
    then do
        (r, ns) <- liftIO $ runStateT (c `commandEval` m) s
        return (ns, r)
    else do
        return (s, Nothing)
    )






pingMod :: Monad m => Module m ()
pingMod = Module
    { moduleSerialize = return Nothing
    , moduleCmds =
        [ Command
            { commandName  = "Ping"
            , commandHelp  = ["<ping>"]
            , commandMatch = \m -> return $ m == "Hi"
            , commandEval  = \m -> return $ Just [m]
            }
        ]
    , moduleInit = return ()
    , moduleExit = return ()
    , moduleName = "Ping"
    }

uptimeMod :: MonadIO m => Module m ClockTime
uptimeMod = Module
    { moduleSerialize = return Nothing
    , moduleCmds =
        [ Command
            { commandName  = "uptime"
            , commandHelp  = ["<uptime>"]
            , commandMatch = \m -> return $ m == "bye"
            , commandEval  = \_ -> do
                past <- get
                now <- liftIO getClockTime
                return $ Just [pretty $ diffClockTimes now past]
            }
        ]
    , moduleInit = liftIO getClockTime
    , moduleExit = return ()
    , moduleName = "Uptime"
    }





-- TODO:
--  - Find a way to share state between a CommandRef and ModuleRef list so
--      that we can iterate through commandref and check for matches + run
--      commands, but upon init & shutdown we can hit ModuleRef to init/cleanup
--  - There maybe a better approach such as a single list that holds all of
--      the needed info and building a nice ... transversable interface for
--      transversing the list for modules vs commands
--
--  - Look at - http://golang.org/pkg/net/http/#ServeMux
--      * You create a serveMux, then register a pattern + handler
--      * it iterates through the pattern till it finds a match, which it then calls the assocated handle
--      * The clever bit is that ServeMux is also a handle so it can call a nested ServeMux
--      $ This current system is just a linear list of matches, no nested matches
--      $ Need to find a clever way of doing that, need to decouple matchers a bit from the handlers
--      $ Main trick is how to share the state so that matchers can look up the state (do we need/want this?)
--
--      * ok, for stand-alone commands/matches, i can just do "addMatch(match, command)" for ex, and for the shared module level,
--          if i make the this mux thing nestable, i could just define a mux in the module level and add handler+match there
--      * for that group, and have nested mux which would achieve both goals of decoupling matches from command, and be able to
--          have modules of shared commands (for shared states)






--
-- Routing Experiments
--

-- Server, Nick, Content (Test message for the matcher)
data Message = Message String String String

-- Type for Match
newtype HandlerT s m a = HandlerT
    { runHandler :: s -> Message -> m (Either [Msg] a, s) }

instance Functor m => Functor (HandlerT s m) where
  fmap f (HandlerT act) = HandlerT $ \st0 req ->
    go `fmap` act st0 req
    where go (eaf, st) = case eaf of
                              Left resp -> (Left resp, st)
                              Right result -> (Right $ f result, st)

instance Monad m => Monad (HandlerT s m) where
  return a = HandlerT $ \st _ -> return $ (Right a, st)
  (HandlerT act) >>= fn = HandlerT $ \st0 req -> do
    (eres, st) <- act st0 req
    case eres of
      Left resp -> return (Left resp, st)
      Right result -> do
        let (HandlerT fres) = fn result
        fres st req

instance (Monad m, Functor m) => Applicative (HandlerT s m) where
  pure = return
  (<*>) = ap

instance (Functor m, Monad m) => Alternative (HandlerT s m) where
  empty = respond []
  (<|>) = (>>)

instance Monad m => MonadPlus (HandlerT s m) where
  mzero = respond []
  mplus = flip (>>)

instance Monad m => MonadState s (HandlerT s m) where
  get = HandlerT $ \s _ -> return (Right s, s)
  put s = HandlerT $ \_ _ -> return (Right (), s)

instance Monad m => MonadReader Message (HandlerT s m) where
  ask = HandlerT $ \st req -> return (Right req, st)
  local f (HandlerT act) = HandlerT $ \st req -> act st (f req)

instance MonadTrans (HandlerT s) where
  lift act = HandlerT $ \st _ -> act >>= \r -> return (Right r, st)

instance MonadIO m => MonadIO (HandlerT s m) where
  liftIO = lift . liftIO

hoistEither :: Monad m => Either [Msg] a -> HandlerT s m a
hoistEither eith = HandlerT $ \st _ -> return (eith, st)

respond :: Monad m => [Msg] -> HandlerT s m a
respond resp = hoistEither $ Left resp

request :: Monad m => HandlerT s m Message
request = ask



--
-- Route
--
guardMessage :: Monad m => (Message -> Bool) -> HandlerT s m a -> HandlerT s m ()
guardMessage f = guardM (liftM f request)

guardM :: Monad m => HandlerT s m Bool -> HandlerT s m a -> HandlerT s m ()
guardM b c = b >>= flip guardH c

guardH :: Monad m => Bool -> HandlerT s m a -> HandlerT s m ()
guardH b c = if b then c >> return () else return ()




type SimpleCommand m = Message -> m [Msg]

-- | Convert the controller into an 'Application'
controllerApp :: Monad m => s -> HandlerT s m a -> SimpleCommand m
controllerApp s ctrl req =
  runHandler ctrl s req >>=
    either return (const $ return []) . fst


test :: HandlerT (HandlerT () IO ()) IO [Msg] -> SimpleCommand IO
test = controllerApp $ do
    guardH True $ do
        guardH False $ do
            respond []
        guardH True $ do
            respond ["Hi"]



--routeTop :: Monad m => HandlerT s m a -> HandlerT s m ()
--routeMatch :: Monad m => (Message -> Bool) -> HandlerT s m a -> HandlerT s m ()


--    , commandMatch :: Msg -> ReaderT st m Bool
--    , commandEval  :: Msg -> StateT st m (Maybe [Msg])
--data ModuleRef m = forall st. ModuleRef (Module m st) (MVar st)
--data CommandRef m = forall st. CommandRef (Command m st) (MVar st)


--runCommands :: Msg -> [CommandRef IO] -> IO (Maybe [Msg])
--runCommands m cs = listToMaybe <$> catMaybes <$> mapM (runCommand m) cs
--
--runCommand :: Msg -> CommandRef IO -> IO (Maybe [Msg])
--runCommand m (CommandRef c st) = liftIO $ modifyMVar st (\s -> do
--    matched <- liftIO $ runReaderT (c `commandMatch` m) s
--    if matched
--    then do
--        (r, ns) <- liftIO $ runStateT (c `commandEval` m) s
--        return (ns, r)
--    else do
--        return (s, Nothing)
--    )









--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s

