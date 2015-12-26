module Tests.KarmatorRoute
    ( routingTests
    ) where
import Test.HUnit

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import qualified Network.IRC as IRC

import Karmator.Types
import Karmator.Route
import Karmator.Bot

--
-- Test Case exports
--
routingTests = TestLabel "Routing Tests" (buildRouteTests routeData)

--
-- Tests
--
buildRouteTests = TestList . map (\(input, result) -> TestLabel (show input ++ " -> " ++ show result) (routeTest input result))

-- TODO: non-ideal to compare the output of an handle but it'll do for now
routeTest input result = TestCase $ do
    run  <- runRoute testRoute input
    run' <- executeCmdRef run input
    assertEqual "" result run'

-- TODO: build a gathering of some nice pattern for testing + output
routeData :: [(BotEvent, [[BotCommand]])]
routeData =
    [ ( EMessage "" $ IRC.Message (Just $ IRC.NickName (C8.pack "") (Just $ C8.pack "never") (Just $ C8.pack "base")) (C8.pack "") [C8.pack "target"], [] )
    ]

-- TODO: Build some more through routing tests for various cases
-- TODO: add persistance and stateful persistance tests
testRoute :: Route [CmdHandler]
testRoute = choice
    [ do
        debug "bye handler"
        pureHandler "bye" (\_ -> return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "bye"])
    , do
        match (\a -> testServer a == "test")
        debug "server handler"
        stateHandler "server" "string" (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "server"]))
    , do
        match (\a -> testServer a == "base")
        debug "base choice"
        choice
            [ do
                match (\a -> channel a == "target")
                match (\a -> nick a == "never")
                debug "never handler"
                stateHandler "never" (3.14 :: Float) (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "never"]))
            , do
                debug "base handler"
                stateHandler "base" (1 :: Integer) (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "base"]))
            , stateHandler "bar" (1 :: Integer) (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "bar"]))
            , return [SCmdRef "return" (2 :: Integer) (\_ _ -> (return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "return"]))]
            ]
    , do
        match (\a -> testServer a == "base")
        pureHandler "io" fooIO
    ]
  where
    testServer :: BotEvent -> String
    testServer (EMessage "" IRC.Message{IRC.msg_prefix=(Just (IRC.Server n))}) = C8.unpack n
    testServer (EMessage "" IRC.Message{IRC.msg_prefix=(Just (IRC.NickName _ _ (Just n)))}) = C8.unpack n
    testServer _ = "" -- TODO: bad

    -- TODO: unsafe head, and does not support multi-channel privmsg
    channel :: BotEvent -> String
    channel (EMessage _ m) = C8.unpack $ head $ IRC.msg_params m
    channel _              = "" -- TODO: bad

    nick :: BotEvent -> String
    nick (EMessage _ IRC.Message{IRC.msg_prefix=(Just (IRC.NickName n _ _))}) = C8.unpack n
    nick _ = ""

    fooIO :: MonadIO m => a -> m [BotCommand]
    fooIO _ = do
        liftIO $ putStrLn "test"
        return $ Just $ CMessage $ IRC.Message Nothing (C8.pack "PRIVMSG") [C8.pack "io"]
