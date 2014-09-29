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

    , executeCmdRef
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Text.PrettyPrint.HughesPJ
import Text.Show.Functions()


-- TODO: bad
import qualified Data.ByteString.Char8 as C8

import qualified Network.IRC as IRC
import Karmator.Types


-- | Match on a message using a predicate
match :: (BotEvent -> Bool) -> Route ()
match p = liftF (Match p ())

-- | Try several routes, using all that succeeds
choice :: [Route a] -> Route a
choice a = join $ liftF (Choice a)

-- | Register a handler
-- TODO: see if we can't refine the type a bit more? (esp m1 o -> m2 a)
handler :: MonadFree (Segment m1 i o) m2 => String -> st -> (st -> i -> m1 o) -> m2 a
handler n s h = liftF (Handler $ CmdRef n s h)

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



--
--
-- TODO: Migrate this stuff to a test suite, but for now it works here as
-- a test case
--
--
route2Free :: Route [CmdHandler]
route2Free = choice
    [ do
        debug "bye handler"
        handler "bye" () (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "bye"]))
    , do
        match (\a -> server a == "test")
        debug "server handler"
        handler "server" "string" (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "server"]))
    , do
        match (\a -> server a == "base")
        debug "base choice"
        choice
            [ do
                match (\a -> channel a == "target")
                match (\a -> nick a == "never")
                debug "never handler"
                handler "never" 3.14 (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "never"]))
            , do
                debug "base handler"
                handler "base" 1 (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "base"]))
            , handler "bar" 1 (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "bar"]))
            , return [CmdRef "return" 2 (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "return"]))]
            ]
    , do
        match (\a -> server a == "base")
        handler "io" () fooIO
    ]
  where
    server :: BotEvent -> String
    server (EMessage IRC.Message{IRC.msg_prefix=(Just (IRC.Server n))}) = C8.unpack n
    server (EMessage IRC.Message{IRC.msg_prefix=(Just (IRC.NickName _ _ (Just n)))}) = C8.unpack n
    server _ = "" -- TODO: bad

    -- TODO: unsafe head, and does not support multi-channel privmsg
    channel :: BotEvent -> String
    channel (EMessage m) = C8.unpack $ head $ IRC.msg_params m

    nick :: BotEvent -> String
    nick (EMessage IRC.Message{IRC.msg_prefix=(Just (IRC.NickName n _ _))}) = C8.unpack n
    nick _ = ""

    fooIO :: MonadIO m => a -> b -> m (Maybe BotCommand)
    fooIO _ _ = do
        liftIO $ putStrLn "test"
        return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "io"]

test :: IO [Maybe BotCommand]
test = do
    let m = EMessage $ IRC.Message (Just $ IRC.NickName (C8.pack "") (Just $ C8.pack "never") (Just $ C8.pack "base")) (C8.pack "") [C8.pack "target"]
    ref <- runRoute route2Free m
    print ref
    executeCmdRef ref m

testDebug :: IO [Maybe BotCommand]
testDebug = do
    let m = EMessage $ IRC.Message (Just $ IRC.NickName (C8.pack "") (Just $ C8.pack "never") (Just $ C8.pack "base")) (C8.pack "") [C8.pack "target"]
    (doc, ref) <- debugRoute route2Free m
    print doc
    print ref
    executeCmdRef ref m

-- TODO: find better home for this
executeCmdRef :: [CmdHandler] -> BotEvent -> IO [Maybe BotCommand]
executeCmdRef cs m = mapM (\(CmdRef _ st h) -> h st m) cs
