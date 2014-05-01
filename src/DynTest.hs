{-# LANGUAGE Rank2Types, DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction, OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
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
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

import System.Console.Haskeline.MonadException

import Control.Concurrent (MVar)


-- TEMPLATE HASKELL
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax

moduleName = fmap loc_module qLocation >>= \mod -> return (AppE (VarE (mkName "Data.ByteString.Char8.pack")) (LitE (StringL mod)))
-- TEMPLATE HASKELL

type Msg = String


--
-- Generation Five
-- TODO: consider breaking it apart some more to decouple the state/other
--       action from the plugin definition maybe
--
data Plug = forall st. Plug
    { _this  :: st
    , _init  :: Maybe B.ByteString -> st
    , _save  :: st -> Maybe B.ByteString
    , _match :: Msg -> st -> Bool
    , _eval  :: Msg -> st -> (st, Maybe Msg)
    }

init :: Maybe B.ByteString -> Plug -> Plug
init st (Plug _ i s m e) = Plug (i st) i s m e

save :: Plug -> Maybe B.ByteString
save (Plug t _ s _ _) = s t

match :: Plug -> Msg -> Bool
match (Plug t _ _ m _) msg = m msg t

eval :: Plug -> Msg -> (Plug, Maybe Msg)
eval (Plug t i s m e) msg = do
    let (st, ms) = e msg t
    (Plug st i s m e, ms)



--
-- Generation Three
--

-- Plugin Monad
newtype PM a = PM { runPM :: ReaderT (String) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadException)

-- | This transformer encodes the additional information a module might
--   need to access its name or its state.
newtype PluginT st m a = PluginT { runPluginT :: ReaderT (MVar st, B.ByteString) m a }
    deriving (Applicative, Functor, Monad, MonadTrans, MonadIO, MonadException)

-- Deals in serialization of the state for the plugin
-- Consider doing some serialization with acid-state
data Serial st = Serial
    { serialize   :: st -> Maybe B.ByteString
    , deserialize :: B.ByteString -> Maybe st
    }

-- Deals with pattern matching a match to a eval
data Command m = Command
    { commandName  :: String
    , commandHelp  :: Msg -> WriterT [Msg] m ()
    , commandMatch :: Msg -> m Bool
    , commandEval  :: Msg -> WriterT [Msg] m ()
    }

-- Plugin structure, for a collection of shared commands
-- TODO: Maybe the monadic structure is a bit too complicated
data Plugin st = Plugin
    -- If want its state to be saved on shutdown return a serializer, returns Nothing in default impl
    { pluginSerialize :: !(Maybe (Serial st))

    -- Default state, returns error in default impl
    , pluginDefState :: !(PM st)

    -- Initialize
    , pluginInit :: !(PluginT st PM ())

    -- Finalize
    , pluginExit :: !(PluginT st PM ())

    -- Listeners
    , pluginListeners :: !(PluginT st PM [Command (PluginT st PM)])

    -- TODO: do we want contextual listeners who can response to raw stream IRC Messages
    }


--
-- Plugin Constructor
--
newPlugin :: Plugin st
newPlugin = Plugin
    { pluginSerialize = Nothing
    , pluginDefState  = return $ error "State not initalized"
    , pluginInit      = return ()
    , pluginExit      = return ()
    , pluginListeners = return []
    }



--
-- Generation Four
--
data ExtensibleState = ExtensibleState (Map.Map String (Either String StateExtension))


-- ---------------------------------------------------------------------
-- Extensible state
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    -- | Defines an initial value for the state extension
    initialValue :: a
    -- | Specifies whether the state extension should be
    -- persistent. Setting this method to 'PersistentExtension'
    -- will make the stored data survive restarts, but
    -- requires a to be an instance of Read and Show.
    --
    -- It defaults to 'StateExtension', i.e. no persistence.
    extensionType :: a -> StateExtension
    extensionType = StateExtension

-- | Existential type to store a state extension.
data StateExtension =
    forall a. ExtensionClass a => StateExtension a
    -- ^ Non-persistent state extension
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a
    -- ^ Persistent extension



--
-- Generation Two
--
deriving instance Typeable ClockTime
data PluginPlus = PluginPlus
    -- Plugin Name
    { nameP :: B.ByteString

    -- Either initalize or loads a persisted state
    , initP :: (Monad f, MonadIO f) => Maybe B.ByteString -> f Dynamic

    -- Saves the persist state
    , saveP :: Dynamic -> B.ByteString

    -- Does the message match this plugin
    , matchP :: Msg -> State Dynamic Bool

    -- Evalulate the message and optionally return a result
    , evalP :: (Monad f, MonadIO f) => Msg -> StateT Dynamic f (Maybe Msg)
    }

-- Test implementation
pingP = PluginPlus pingName pingInit pingSave pingMatch pingEval
    where
        pingName    = "Ping"
        pingInit _  = return $ toDyn ()
        pingSave _  = B.empty
        pingMatch m = return $ m == "Hi"
        pingEval m  = return $ Just m

uptimeP = PluginPlus uptimeName uptimeInit uptimeSave uptimeMatch uptimeEval
    where
        uptimeName    = "Uptime"
        uptimeInit _  = liftM toDyn (liftIO getClockTime)
        uptimeSave _  = B.empty
        uptimeMatch m = return $ m == "Bye"
        uptimeEval m  = do
            now  <- liftIO getClockTime
            past <- get

            return $ case fromDynamic past of
                Nothing -> Nothing
                Just p  -> Just $ pretty $ diffClockTimes now p


-- Supporting code for running and managing the state
pluginP :: [PluginPlus]
pluginP = [pingP, uptimeP]

-- TODO: Probably want to have sanity check or a way to enforce nameP to be unique
initEmptyPluginData :: (Functor f, Monad f, MonadIO f) => [PluginPlus] -> f (Map.Map B.ByteString Dynamic)
initEmptyPluginData l = liftM Map.fromList $ zip (map nameP l) `fmap` mapM (`initP` Nothing) l


--
-- Take advantage of lazy eval and maybe consider revert state to lazy
-- We just need to feed this into everything (maybe problemic with IO, need
-- double check)
commandP :: (Monad f, MonadIO f)
         => [PluginPlus]
         -> Map.Map B.ByteString Dynamic
         -> Msg
         -> f (Maybe Msg, Map.Map B.ByteString Dynamic)
commandP s m = undefined
--
-- Need a neat way to be able to take a list of match rule then upon the
-- first or whatever match, pass it on to the eval to evalulate it
--
-- I wonder if there's no neat way to just iterate through the plugin list
-- then run the match and then if it matches then eval then keep going... ?
--



--    -- Does the message match this plugin
--    , matchP :: Msg -> State Dynamic Bool
--
--    -- Evalulate the message and optionally return a result
--    , evalP :: (Monad f, MonadIO f) => Msg -> StateT Dynamic f (Maybe Msg)
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

