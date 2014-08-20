{-# LANGUAGE ExistentialQuantification, TemplateHaskell, NoMonomorphismRestriction #-}
--{-# LANGUAGE Rank2Types, DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction, OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
import Data.Dynamic
import Data.Typeable

import Data.List hiding (init)
import System.IO
import System.Time
import Control.Monad
import Control.Monad.IO.Class
import Prelude hiding (log, init)
import Data.Maybe
import Control.Applicative

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

import System.Console.Haskeline.MonadException

import Control.Concurrent.MVar

-- Lazy
import System.IO.Unsafe


-- TEMPLATE HASKELL
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax

moduleNameTH = fmap loc_module qLocation >>= \mod -> return (AppE (VarE (mkName "Data.ByteString.Char8.pack")) (LitE (StringL mod)))
-- TEMPLATE HASKELL

type Msg = String



--
-- Final
--

-- Final version that trys to incorporate all of the nice features of all
-- of the other versions
--
data Command m st = Command -- TODO: the st maybe better off as State monad so they can pull/update the state if needed
    { commandHelp  :: st -> Msg -> m [Msg] -- TODO: uncertain, this should be as simple as possible
    , commandMatch :: st -> Msg -> m Bool
    , commandEval  :: st -> Msg -> m (st, Maybe [Msg])
    }

-- Deals in serialization of the state for the plugin
-- TODO: customize it so that I can provide some default serialization such as json, etc
-- TODO: look into doing something like this below v to automate serialization (ie read/show)
--
---- | Existential type to store a state extension.
--data StateExtension =
--    forall a. ExtensionClass a => StateExtension a
--    -- ^ Non-persistent state extension
--  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a
--    -- ^ Persistent extension
--
data Serial st = Serial
    { serialize   :: st -> Maybe B.ByteString
    , deserialize :: B.ByteString -> Maybe st
    }

-- Module that contains the plugins
data Module m st = Module
    { moduleSerialize :: m (Maybe (Serial st))
    , moduleCmds :: m [Command m st]

    -- TODO: not certain how to handle init/exit yet
    --, pluginDefState  = return $ error "State not initalized"
    , moduleDefState :: m st

    -- TODO: this should be handled by some template haskell maybe
    , moduleName :: String
    }

instance Show (Module m st) where
    show (Module _ _ _ name) = name

data ModuleRef m = forall st. ModuleRef (Module m st) (MVar st)
data CommandRef m = forall st. CommandRef (Command m st) (MVar st)

instance Show (ModuleRef m) where
    show (ModuleRef m _) = "Module: " ++ show m ++ " state: -"

instance Show (CommandRef m) where
    show (CommandRef _ _) = "Command: - state: -"

--
-- This registers a module defined above into a format suitable for
-- processing by the ircbot in a generic manner
--
loadModule :: (Monad m, MonadIO m) => Module m st -> m (ModuleRef m, [CommandRef m])
loadModule m = do
    state' <- moduleDefState m
    ref <- liftIO $ newMVar state'
    cmdlist <- moduleCmds m

    let cmdref = map (\c -> CommandRef c ref) cmdlist

    return (ModuleRef m ref, cmdref)



-- Test plugin
--data Command m st = Command
--    { commandHelp  :: st -> Msg -> m [Msg]
--    , commandMatch :: st -> Msg -> m Bool
--    , commandEval  :: st -> Msg -> m (st, Maybe [Msg])
--    }

pingMod :: Monad m => Module m ()
pingMod = Module
    { moduleSerialize = return Nothing
    , moduleCmds = return
        [ Command
            { commandHelp  = \_ m -> return $ ["<ping>"]
            , commandMatch = \_ m -> return $ m == "Hi"
            , commandEval  = \_ m -> return $ ((), Just [m])
            }
        ]
    , moduleDefState = return ()
    , moduleName = "Ping"
    }

uptimeMod :: MonadIO m => Module m ClockTime
uptimeMod = Module
    { moduleSerialize = return Nothing
    , moduleCmds = return
        [ Command
            { commandHelp  = \_ m -> return $ ["<uptime>"]
            , commandMatch = \_ m -> return $ m == "bye"
            , commandEval  = \p m -> do
                now <- liftIO getClockTime
                return (p, Just [pretty $ diffClockTimes now p])
            }
        ]
    , moduleDefState = liftIO getClockTime
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
-- Generation Five
-- TODO: consider breaking it apart some more to decouple the state/other
--       action from the plugin definition maybe
--
--data Plug = forall st. Plug
--    { _this  :: st
--    , _init  :: Maybe B.ByteString -> st
--    , _save  :: st -> Maybe B.ByteString
--    , _match :: Msg -> st -> Bool
--    , _eval  :: Msg -> st -> (st, Maybe Msg)
--    }
--    , evalP :: (Monad f, MonadIO f) => Msg -> StateT Dynamic f (Maybe Msg)

-- TODO: not the biggest fan of undefined by default, maybe good to find
-- a way to "provide" the plugin and build the existential qualification
--emptyPlug = Plug undefined
--
--init :: Maybe B.ByteString -> Plug -> Plug
--init st (Plug _ i s m e) = Plug (i st) i s m e
--
--save :: Plug -> Maybe B.ByteString
--save (Plug t _ s _ _) = s t
--
--match :: Plug -> Msg -> Bool
--match (Plug t _ _ m _) msg = m msg t
--
--eval :: Plug -> Msg -> (Plug, Maybe Msg)
--eval (Plug t i s m e) msg = do
--    let (st, ms) = e msg t
--    (Plug st i s m e, ms)
--
--
--pingPlug = emptyPlug pingInit pingSave pingMatch pingEval
--    where
--        pingInit _ = ()
--        pingSave _ = Nothing
--        pingMatch m _ = m == "Hi"
--        pingEval m _ = ((), Just m)
--
--uptimePlug = emptyPlug uptimeInit uptimeSave uptimeMatch uptimeEval
--    where
--        uptimeInit _ = unsafePerformIO $ liftIO getClockTime
--        uptimeSave _ = Nothing
--        uptimeMatch m _ = m == "Bye"
--        uptimeEval m past = unsafePerformIO $ do
--            now  <- liftIO getClockTime
--            return (past, Just $ pretty $ diffClockTimes now past)





--    -- TODO: look into seeing if there's a way to setup some form of
--    -- generic filtering rules such as "if chan = y send to x", etc..
--    -- Perhaps Pipe.Prelude.Filter
--    result <- catMaybes <$> forM
--        [ \a -> return $ if pingMatch a then ping a else Nothing
--        , \a -> return $ if motdMatch a then motdJoin a else Nothing
--        , \a -> if uptimeMatch a then uptime t a else return $ Nothing -- IO
--        ]
--        (\a -> a msg)
--
--    -- TODO: Unsafe head
--    unless (null result) (yield $ head result)





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

