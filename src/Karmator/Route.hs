{-# LANGUAGE ExistentialQuantification, GADTs, DeriveFunctor, FlexibleInstances, FlexibleContexts #-}
module Karmator.Route
    -- TODO: clean up the type export and restrict it
    ( CmdRef(..)
    , Route(..)

    , match
    , choice
    , handler
    , debug
    , runRoute
    , debugRoute
    ) where

-- TODO: fix this import (we don't use reader but we need MonadTrans)
import Control.Monad.Reader

import Control.Concurrent.MVar
import Text.Show.Functions
import qualified Control.Monad.Trans.Free as F
import Text.PrettyPrint.HughesPJ (Doc, (<+>), ($+$), (<>), char, doubleQuotes, nest, space, text, vcat, empty)


-- TODO: replace with IRC.Message
data Message = Message
    { server :: String
    , channel :: String
    , nick :: String
    , msg :: String
    }
    deriving Show


data Segment i o n
    = Match (i -> Bool) n
    | Choice [n]
    | Handler (CmdRef i o)
    deriving (Functor, Show)


-- TODO: Replace bare state with (MVar state) or something, maybe even a hook to the plugin's module level stuff
--data CommandRef m = forall st. CommandRef (Command m st) (MVar st)
data CmdRef i o = forall st. CmdRef String st (st -> i -> o)
instance Show (CmdRef i o) where
    show (CmdRef n _ _) = "Command: " ++ show n

-- TODO: newtype
type Route a = F.FreeT (Segment Message String) IO a


-- | Match on a message using a predicate
match :: (Message -> Bool) -> Route ()
match p = F.liftF (Match p ())

-- | Try several routes, using all that succeeds
choice :: [Route a] -> Route a
choice a = join $ F.liftF (Choice a)

-- | Register a handler
handler :: F.MonadFree (Segment i o) m => String -> st -> (st -> i -> o) -> m a
handler n s h = F.liftF (Handler $ CmdRef n s h)

-- | Non-route that prints debugging info
debug :: MonadTrans t => String -> t IO ()
debug = lift . putStrLn

-- | Run the route
runRoute :: Route [CmdRef Message String] -> Message -> IO [CmdRef Message String]
runRoute f m = do
    x <- F.runFreeT f
    case x of
        -- TODO: this makes no sense
        (F.Pure a)              -> return a
        (F.Free (Match p r))    -> if p m then runRoute r m else return []
        (F.Free (Choice c))     -> fmap concat $ mapM (flip runRoute m) c
        (F.Free (Handler h))    -> return [h]

-- | Debug interpreter for running route
debugRoute :: Route [CmdRef Message String] -> Message -> IO (Doc, [CmdRef Message String])
debugRoute f m = do
    x <- F.runFreeT f
    case x of
        (F.Pure a)              -> return (text "Pure" <+> (text $ show a), a)
        (F.Free (Match p r))    ->
            if p m
            then do
                (doc, ma) <- debugRoute r m
                return (text "match <predicate> -- matched" <+> (text $ show m) $+$ doc, ma)
            else do
                return (text "match <predicate> -- did not match" <+> (text $ show m) $+$ text "-- aborted", [])
        (F.Free (Choice c))     -> do
            sub <- mapM (flip debugRoute m) c
            let (docs, ma) = foldr debugs ([], []) sub
            return (text "choice" <+> showPrettyList (map (\d -> text "do" <+> d) docs), ma)
        (F.Free (Handler h))    -> return (text "Handler" <+> (text $ show h), [h])
  where
    debugs :: (a, [t]) -> ([a], [t]) -> ([a], [t])
    debugs (doc, []) (docs, [])  = (doc:docs, [])
    debugs (doc, a)  (docs, [])  = (doc:docs, a)
    debugs (doc, []) (docs, a)   = (doc:docs, a)
    debugs (doc, a)  (docs, b)   = (doc:docs, a++b)

    showPrettyList  :: [Doc] -> Doc
    showPrettyList []     = text "[]"
    showPrettyList [x]    = char '[' <+> x $+$ char ']'
    showPrettyList (h:tl) = char '[' <+> h $+$ (vcat (map showTail tl)) $+$ char ']'
      where
        showTail x = char ',' <+> x















-- TODO: remove or move this stuff into a test module for testing this route stuff
route2Free :: Route [CmdRef Message String]
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
                return [CmdRef "return" 2 (\_ _ -> "return")]
            ]
    ]


executeCmdRef :: [CmdRef Message String] -> Message -> IO [String]
executeCmdRef cs m = mapM (\(CmdRef _ st h) -> return $ h st m) cs

test :: IO [String]
test = do
    let m = Message "base" "target" "never" "d"
    ref <- runRoute route2Free m
    putStrLn $ show ref
    executeCmdRef ref m

testDebug :: IO [String]
testDebug = do
    let m = Message "base" "target" "never" "d"
    (doc, ref) <- debugRoute route2Free m
    putStrLn $ show doc
    putStrLn $ show ref
    executeCmdRef ref m
