{-# LANGUAGE ExistentialQuantification, GADTs, DeriveFunctor, FlexibleInstances, FlexibleContexts #-}

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

import Text.Show.Functions
import Control.Monad.Free (Free(Pure, Free), liftF)

import qualified Control.Monad.Trans.Free as F

import Text.PrettyPrint.HughesPJ (Doc, (<+>), ($+$), (<>), char, doubleQuotes, nest, space, text, vcat, empty)


-- TEMPLATE HASKELL
import Language.Haskell.TH hiding (match, Match)
import Language.Haskell.TH.Syntax hiding (lift, Match, Module)

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

--
-- TODO:
--  1) Reimplement Free into terms of FreeT so we can bring in State for
--      reading/writing stateful computation
--  2) Bring in some form of Monad/MonadIO to support IO action in the
--      routing handlers
--  3) Identify how to make the (get/set) be Existential so that we can
--      have routers with all sort of states
--  4) Write a simple interpreter that can take care of the routing + state
--      for the handles
--  5) Recursive handlers

data Message = Message
    { server :: String
    , channel :: String
    , nick :: String
    , msg :: String
    }

--
-- Define exactly what needs to happen here.
--
-- Handle = One handler + its wrapped state-ref
-- Match = (Message -> Bool)
--
-- a = handle1  -- always execute this one handler
-- b = [ handle1, handle2 ] -- Always execute these two handler
-- c = [ Match -> Handle1, Match -> Handle2 ] -- Match then execute matched handler
-- d = [ Match -> [ Match -> Handle1 ], Match -> Handle2 ] -- Match then descend into pseudo handler to handle further matches
-- e = [ Match -> Match -> Handle1, [ Match -> [ Match -> Handle2 ], Handle3 ] ] -- Nested matches and mixed matches+handles

--data Segment a where
--    Handler :: forall st. (st -> Message -> String) -> st   -> Segment String
--    Match   :: Show a => (Message -> Bool) -> a             -> Segment a
--    Choice  :: Show a => [a]                                -> Segment a

data Segment i o n
    = Match (i -> Bool) n
    | Choice [n]
    | Handler CmdRef
    deriving (Functor, Show)

-- name, state, state-process
--data CommandRef m = forall st. CommandRef (Command m st) (MVar st)
data CmdRef = forall st. CmdRef String st (st -> Message -> String)
instance Show CmdRef where
    show (CmdRef n _ _) = "Command: " ++ show n


-- TODO: newtype
type Route a = F.FreeT (Segment Message String) IO a

-- | Match on a message using a predicate
--match :: F.MonadFree (Segment i o) m => (i -> Bool) -> m ()
match :: (Message -> Bool) -> Route ()
match p = F.liftF (Match p ())

-- | Try several routes, using all that succeeds
--choice :: F.MonadFree (Segment i o) m => [m a] -> m a
choice :: [Route a] -> Route a
choice a = join $ F.liftF (Choice a)

-- | Register a handler
--handler :: F.MonadFree (Segment i o) m => st -> (st -> i -> o) -> m a
handler :: F.MonadFree (Segment i o) m => String -> st -> (st -> Message -> String) -> m a
handler n s h = F.liftF (Handler $ CmdRef n s h)

-- | Non-route that prints debugging info
debug :: MonadTrans t => String -> t IO ()
debug = lift . putStrLn

-- | Run the route
runRoute :: Route [String] -> Message -> IO [String]
runRoute f m = do
    x <- F.runFreeT f
    case x of
        -- TODO: this makes no sense
        (F.Pure a)                          -> return a
        (F.Free (Match p r))                -> if p m then runRoute r m else return []
        (F.Free (Choice c))                 -> fmap concat $ mapM (flip runRoute m) c
        (F.Free (Handler (CmdRef n st h)))  -> return [h st m]

-- | Test route
route2Free :: Route [String]
route2Free = choice
    [ do
        debug "bye handler"
        handler "bye" () (\_ _ -> "bye")
    , do
        match (\a -> server a == "test")
        debug "server handler"
        handler "server" "string" (\_ _ -> "server")
    , do
        match (\a -> server a == "base")
        debug "base choice"
        choice
            [ do
                match (\a -> channel a == "target")
                match (\a -> nick a == "never")
                debug "never handler"
                handler "never" (3.14) (\_ _ -> "never")
            , do
                debug "base handler"
                handler "base" 1 (\_ _ -> "base")
            , handler "bar" 1 (\_ _ -> "bar")
            , do
                return ["hi"]
            ]
    ]

test :: IO [String]
test = runRoute route2Free (Message "base" "target" "never" "d")


---- | run a route, also returning debug log
--debugRoute :: Show a => UrlRoute a -> [String] -> (Doc, [a])
--debugRoute (Pure a) _ =
--    (text "Pure [" <+> (text $ show a) <+> text "]", [a])
--debugRoute (Free (UrlMatch p' r)) (p:ps)
--    | p' p =
--        let (doc, ma) = debugRoute r ps
--        in (text "dir" <+> text "<predicate>" <+> text "-- matched" <+> text p $+$ doc, ma)
--    | otherwise =
--       (text "dir" <+> text "<predicate>" <+> text "-- did not match" <+> text p $+$ text "-- aborted", [])
--debugRoute (Free (Capture convert)) (p:ps) =
--   case convert p of
--     Nothing  -> (text "path <func>" <+> text "-- was not able to convert" <+> text p $+$ text "-- aborted", [])
--     (Just r) ->
--         let (doc, ma) = debugRoute r ps
--         in (text "path <func>" <+> text "-- matched" <+> text p $+$ doc, ma)
--debugRoute (Free (Set val r)) ps =
--    let (doc, ma) = debugRoute r ps
--    in (text "saved" <+> text val <+> text "-- saved" $+$ doc, ma)
--debugRoute (Free (Get r)) ps =
--    let (doc, ma) = debugRoute (r "Test") ps
--    in (text "get" <+> text (show (r "Test")) <+> text "-- loaded" $+$ doc, ma) -- TODO: nice to be able to display the value in the get somehow
--debugRoute (Free (UrlChoice choices)) paths =
--    let debugs (doc, []) (docs, [])  = (doc:docs, [])
--        debugs (doc, a)  (docs, [])  = (doc:docs, a)
--        debugs (doc, []) (docs, a)   = (doc:docs, a)
--        debugs (doc, a)  (docs, b)   = (doc:docs, b++a) -- flipped to match msum in runAllRoutes
--
--        (docs, ma) = foldr debugs ([], []) $ reverse $ map (flip debugRoute paths) choices
--   in (text "choice" <+> showPrettyList (map (\d -> text "do" <+> d) $ reverse docs), ma)
--debugRoute (Free UrlZero) _ =
--    (text "zero", [])
--debugRoute _ [] =
--    (text "-- ran out of path segments before finding 'Pure'", [])
--
--showPrettyList  :: [Doc] -> Doc
--showPrettyList []     = text "[]"
--showPrettyList [x]    = char '[' <+> x $+$ char ']'
--showPrettyList (h:tl) = char '[' <+> h $+$ (vcat (map showTail tl)) $+$ char ']'
--    where
--     showTail x = char ',' <+> x
