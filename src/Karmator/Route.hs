{-# LANGUAGE DeriveFunctor, FlexibleInstances, FlexibleContexts #-}
module Karmator.Route
    -- TODO: clean up the type export and restrict it
    ( Route(..)

    , match
    , choice
    , handler
    , debug
    , runRoute
    , debugRoute
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Text.PrettyPrint.HughesPJ
import Text.Show.Functions

-- TODO: bad
import qualified Data.ByteString.Char8 as C8

import qualified Network.IRC as IRC

-- Karmator Stuff
import Karmator.Types

-- | The heart of the Route
data Segment i o n
    = Match (i -> Bool) n
    | Choice [n]
    | Handler (CmdRef i o)
    deriving (Functor, Show)

-- | Newtype of the freeT transformer stack
type Route m a = FreeT (Segment IRC.Message (Maybe IRC.Message)) m a
--newtype Route m a = FreeT (Segment IRC.Message (Maybe IRC.Message)) m a
type CmdHandler = CmdRef IRC.Message (Maybe IRC.Message)

-- | Match on a message using a predicate
match :: (Monad m) => (IRC.Message -> Bool) -> Route m ()
match p = liftF (Match p ())

-- | Try several routes, using all that succeeds
choice :: (Monad m) => [Route m a] -> Route m a
choice a = join $ liftF (Choice a)

-- | Register a handler
handler :: MonadFree (Segment i o) m => String -> st -> (st -> i -> o) -> m a
handler n s h = liftF (Handler $ CmdRef n s h)

-- | Non-route that prints debugging info
debug :: MonadTrans t => String -> t IO ()
debug = lift . putStrLn

-- | Run the route
runRoute :: (Monad m, Functor m) => Route m [CmdHandler] -> IRC.Message -> m [CmdHandler]
runRoute f m = do
    x <- runFreeT f
    case x of
        -- TODO: this makes no sense
        (Pure a)              -> return a
        (Free (Match p r))    -> if p m then runRoute r m else return []
        (Free (Choice c))     -> fmap concat $ mapM (flip runRoute m) c
        (Free (Handler h))    -> return [h]

-- | Debug interpreter for running route
debugRoute :: (Monad m) => Route m [CmdHandler] -> IRC.Message -> m (Doc, [CmdHandler])
debugRoute f m = do
    x <- runFreeT f
    case x of
        (Pure a)              -> return (text "Pure" <+> (text $ show a), a)
        (Free (Match p r))    ->
            if p m
            then do
                (doc, ma) <- debugRoute r m
                return (text "match <predicate> -- matched" <+> (text $ show m) $+$ doc, ma)
            else do
                return (text "match <predicate> -- did not match" <+> (text $ show m) $+$ text "-- aborted", [])
        (Free (Choice c))     -> do
            sub <- mapM (flip debugRoute m) c
            let (docs, ma) = foldr debugs ([], []) sub
            return (text "choice" <+> showPrettyList (map (\d -> text "do" <+> d) docs), ma)
        (Free (Handler h))    -> return (text "Handler" <+> (text $ show h), [h])
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



--
--
-- TODO: Migrate this stuff to a test suite, but for now it works here as
-- a test case
--
--
route2Free :: Route IO [CmdHandler]
route2Free = choice
    [ do
        debug "bye handler"
        handler "bye" () (\_ _ -> (Just $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "bye"]))
    , do
        match (\a -> server a == "test")
        debug "server handler"
        handler "server" "string" (\_ _ -> (Just $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "server"]))
    , do
        match (\a -> server a == "base")
        debug "base choice"
        choice
            [ do
                match (\a -> channel a == "target")
                match (\a -> nick a == "never")
                debug "never handler"
                handler "never" (3.14) (\_ _ -> (Just $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "never"]))
            , do
                debug "base handler"
                handler "base" 1 (\_ _ -> (Just $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "base"]))
            , handler "bar" 1 (\_ _ -> (Just $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "bar"]))
            , do
                return [CmdRef "return" 2 (\_ _ -> (Just $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "return"]))]
            ]
    ]
  where
    server :: IRC.Message -> String
    server IRC.Message{IRC.msg_prefix=(Just (IRC.Server n))} = C8.unpack n
    server IRC.Message{IRC.msg_prefix=(Just (IRC.NickName _ _ (Just n)))} = C8.unpack n
    server _ = "" -- TODO: bad

    -- TODO: unsafe head, and does not support multi-channel privmsg
    channel :: IRC.Message -> String
    channel = C8.unpack . head . IRC.msg_params

    nick :: IRC.Message -> String
    nick IRC.Message{IRC.msg_prefix=(Just (IRC.NickName n _ _))} = C8.unpack n
    nick _ = ""

executeCmdRef :: [CmdHandler] -> IRC.Message -> IO [(Maybe IRC.Message)]
executeCmdRef cs m = mapM (\(CmdRef _ st h) -> return $ h st m) cs

test :: IO [(Maybe IRC.Message)]
test = do
    let m = IRC.Message (Just $ IRC.NickName (C8.pack "") (Just $ C8.pack "never") (Just $ C8.pack "base")) (C8.pack "") [C8.pack "target"]
    ref <- runRoute route2Free m
    putStrLn $ show ref
    executeCmdRef ref m

testDebug :: IO [(Maybe IRC.Message)]
testDebug = do
    let m = IRC.Message (Just $ IRC.NickName (C8.pack "") (Just $ C8.pack "never") (Just $ C8.pack "base")) (C8.pack "") [C8.pack "target"]
    (doc, ref) <- debugRoute route2Free m
    putStrLn $ show doc
    putStrLn $ show ref
    executeCmdRef ref m
