{-# LANGUAGE FlexibleContexts #-}
module Karmator.Route
    -- TODO: clean up the type export and restrict it
    ( match
    , choice
    , handler
    , debug
    , runRoute
    , debugRoute

    , Route
    , CmdHandler

    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Text.PrettyPrint.HughesPJ
import Text.Show.Functions()

import Karmator.Types


-- | Match on a message using a predicate
match :: (BotEvent -> Bool) -> Route ()
match p = liftF (Match p ())

-- | Try several routes, using all that succeeds
choice :: [Route a] -> Route a
choice a = join $ liftF (Choice a)

-- | Register a handler
-- TODO: see if we can't refine the type a bit more? (esp m1 o -> m2 a)
-- TODO: implement 3 more variant (one with state, one without, one with persistent, and without)
handler :: MonadFree (Segment m1 i o) m2 => String -> st -> p -> (st -> p -> i -> m1 o) -> m2 a
handler n s p h = liftF (Handler $ CmdRef n s p h)

-- | Non-route that prints debugging info
debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

-- | Run the route
-- TODO: refine the functor
runRoute :: Route [CmdHandler] -> BotEvent -> IO [CmdHandler]
runRoute f m = do
    x <- runFreeT f
    case x of
        (Pure a)              -> return a
        (Free (Match p r))    -> if p m then runRoute r m else return []
        (Free (Choice c))     -> concat <$> mapM (`runRoute` m) c
        (Free (Handler h))    -> return [h]

-- | Debug interpreter for running route
debugRoute :: Route [CmdHandler] -> BotEvent -> IO (Doc, [CmdHandler])
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
