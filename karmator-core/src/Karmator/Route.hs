{-# LANGUAGE FlexibleContexts #-}
module Karmator.Route
    -- TODO: clean up the type export and restrict it
    ( match
    , choice
    , pureHandler
    , stateHandler
    , persistHandler
    , persistStateHandler

    , debug
    , runRoute
    , debugRoute

    , Route
    , CmdHandler

    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Text.PrettyPrint.HughesPJ
import Text.Show.Functions()

import Karmator.Types


-- | Match on a message using a predicate
match :: (BotEvent a b -> Bool) -> Route () a b
match p = liftF (Match p ())

-- | Try several routes, using all that succeeds
-- TODO: maybe neat to add in support for varying persistance method, but for now force one
choice :: [Route a b c] -> Route a b c
choice a = join $ liftF (Choice a)

-- | Register a pure handler
-- TODO: see if we can't refine the type a bit more? (esp m1 o -> m2 a)
pureHandler :: MonadFree (Segment m1 p i o) m2 => String -> (i -> m1 o) -> m2 a
pureHandler n h = liftF (Handler $ CmdRef n h)

-- | Register a stateful handler
-- TODO: see if we can't refine the type a bit more? (esp m1 o -> m2 a)
stateHandler :: MonadFree (Segment m1 p i o) m2 => String -> st -> (st -> i -> m1 o) -> m2 a
stateHandler n s h = liftF (Handler $ SCmdRef n s h)

-- | Register a persisting handler
-- TODO: see if we can't refine the type a bit more? (esp m1 o -> m2 a)
persistHandler :: MonadFree (Segment m1 p i o) m2 => String -> p -> (p -> i -> m1 o) -> m2 a
persistHandler n p h = liftF (Handler $ PCmdRef n p h)

-- | Register a persisting stateful handler
-- TODO: see if we can't refine the type a bit more? (esp m1 o -> m2 a)
persistStateHandler :: MonadFree (Segment m1 p i o) m2 => String -> p -> st -> (p -> st -> i -> m1 o) -> m2 a
persistStateHandler n p s h = liftF (Handler $ PSCmdRef n p s h)

-- | Non-route that prints debugging info
debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

-- | Run the route
-- TODO: refine the functor
runRoute :: Route [CmdHandler a b] a b -> BotEvent a b -> IO [CmdHandler a b]
runRoute f m = do
    x <- runFreeT f
    case x of
        (Pure a)              -> return a
        (Free (Match p r))    -> if p m then runRoute r m else return []
        (Free (Choice c))     -> concat <$> mapM (`runRoute` m) c
        (Free (Handler h))    -> return [h]

-- | Debug interpreter for running route
debugRoute :: (Show a, Show b) => Route [CmdHandler a b] a b -> BotEvent a b -> IO (Doc, [CmdHandler a b])
debugRoute f m = do
    x <- runFreeT f
    case x of
        (Pure a)              -> return (text "Pure" <+> text (show a), a)
        (Free (Match p r))    ->
            if p m
            then do
                (doc, ma) <- debugRoute r m
                return (text "match <predicate> -- matched" <+> text (show m) $+$ doc, ma)
            else
                return (text "match <predicate> -- did not match" <+> text (show m) $+$ text "-- aborted", [])
        (Free (Choice c))     -> do
            sub <- mapM (`debugRoute` m) c
            let (docs, ma) = foldr debugs ([], []) sub
            return (text "choice" <+> showPrettyList (map (\d -> text "do" <+> d) docs), ma)
        (Free (Handler h))    -> return (text "Handler" <+> text (show h), [h])
  where
    debugs :: (a, [t]) -> ([a], [t]) -> ([a], [t])
    debugs (doc, []) (docs, [])  = (doc:docs, [])
    debugs (doc, a)  (docs, [])  = (doc:docs, a)
    debugs (doc, []) (docs, a)   = (doc:docs, a)
    debugs (doc, a)  (docs, b)   = (doc:docs, a++b)

    showPrettyList  :: [Doc] -> Doc
    showPrettyList []     = text "[]"
    showPrettyList [x]    = char '[' <+> x $+$ char ']'
    showPrettyList (h:tl) = char '[' <+> h $+$ vcat (map showTail tl) $+$ char ']'
      where
        showTail x = char ',' <+> x
