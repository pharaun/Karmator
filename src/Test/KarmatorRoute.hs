{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import qualified Network.IRC as IRC

import Karmator.Types
import Karmator.Route


--
-- Execute some tests
--
main :: IO ()
main = do
    putStrLn "Routing tests"
    let routeTest = buildRouteTests routeData
    print =<< runTestTT routeTest

buildRouteTests = TestList . map (\(input, result) -> TestLabel (show input ++ " -> " ++ show result) (routeTest input result))

-- TODO: non-ideal to compare the output of an handle but it'll do for now
routeTest input result = TestCase $ do
    run  <- runRoute testRoute input
    run' <- executeCmdRef run input
    assertEqual "" result run'

-- TODO: build a gathering of some nice pattern for testing + output
routeData :: [(BotEvent, [Maybe BotCommand])]
routeData =
    [ ( EMessage $ IRC.Message (Just $ IRC.NickName (C8.pack "") (Just $ C8.pack "never") (Just $ C8.pack "base")) (C8.pack "") [C8.pack "target"], [] )
    ]

-- TODO: Build some more through routing tests for various cases
testRoute :: Route [CmdHandler]
testRoute = choice
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
