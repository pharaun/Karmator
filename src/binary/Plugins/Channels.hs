{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Plugins.Channels
    ( inviteMatch
    , inviteJoin

    , kickMatch
    , kickLeave

    , partMatch
    , partLeave

    , listMatch
    , listChannel

    , motdMatch
    , motdJoin
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Database.Persist.Sql
import Data.ByteString.UTF8 (fromString, toString)
import Data.List.Split (chunksOf)
import Data.Maybe

import Karmator.State
import Karmator.Types
import Karmator.Filter
import qualified Network.IRC as IRC


--autoJoinChannel network = PersistState
--    { plugin = 'moduleName
--    , serialize = showSerialize
--    , deserialize = readDeserialize
--    , key = T.concat [network, ",", "auto_join_channel"]
--    }
--
--modifyState autoJoinChannel (\a -> Set.insert (toString channel) a)

--modifyState :: (MonadIO m) => Text -> (ByteString -> a) -> (b -> ByteString) -> Text -> (a -> b) -> SqlPersistT m (Maybe b)
--showSerialize :: Show a => a -> ByteString
--readDeserialize :: Read a => ByteString -> a -- TODO: should be (Maybe a)


--
-- Invite
--
inviteMatch :: BotEvent -> Bool
inviteMatch             = exactCommand "INVITE"

inviteJoin :: MonadIO m => BotEvent -> ReaderT ConnectionPool m [BotCommand]
inviteJoin (EMessage _ m) = do
    let channel = head $ tail $ IRC.msg_params m

    pool <- ask
    liftIO $ flip runSqlPool pool (do
        c <- modifyState "Plugins.Channels" readDeserialize showSerialize "test.auto_join_channel" (\a -> Set.insert (toString channel) a)
        case c of
            Nothing -> setState "Plugins.Channels" showSerialize "test.auto_join_channel" (Set.singleton $ toString channel)
            Just _  -> return ()
        )

    return [CMessage $ IRC.joinChan channel]
inviteJoin _ = return []


--
-- Part & Kick
--
kickMatch = exactCommand "KICK"
kickLeave (EMessage _ m) = do
    let channel = head $ IRC.msg_params m

    pool <- ask
    liftIO $ flip runSqlPool pool (do
        c <- modifyState "Plugins.Channels" readDeserialize showSerialize "test.auto_join_channel" (\a -> Set.delete (toString channel) a)
        case c of
            Nothing -> return () -- deleteState "Plugins.Channels" "test.auto_join_channel"
            Just _  -> return ()
        )
    return []
kickLeave _ = return []

partMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!part")
partLeave (EMessage _ m) = do
    let channel = head $ IRC.msg_params m

    pool <- ask
    liftIO $ flip runSqlPool pool (do
        c <- modifyState "Plugins.Channels" readDeserialize showSerialize "test.auto_join_channel" (\a -> Set.delete (toString channel) a)
        case c of
            Nothing -> return () -- deleteState "Plugins.Channels" "test.auto_join_channel"
            Just _  -> return ()
        )

    return [CMessage $ IRC.part channel]
partLeave _ = return []

--
-- List of channels to join
--
listMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!list")
listChannel m = do
    pool <- ask
    chan <- liftIO $ flip runSqlPool pool (getState "Plugins.Channels" readDeserialize "test.auto_join_channel")

    return [CMessage $ IRC.privmsg (whichChannel m) (fromString $ show (chan :: Maybe (Set String)))]


--
-- Motd Join
-- TODO: add support for "auth ping/pong" before registering/joining channels
--
-- TODO: make channel length + throttle be a config for now we join 3 at
-- a time once each second
--
-- TODO: make network aware
--
motdMatch n m = exactCommand "004" m && networkMatch n m

motdJoin _ cs _ = do
    pool <- ask
    -- TODO: replace 'test' with n
    chan <- liftIO $ flip runSqlPool pool (getState "Plugins.Channels" readDeserialize "test.auto_join_channel")
    let chunks = map (BS.intercalate ",") $ chunksOf 3 (cs ++ (fromMaybe [] chan))

    -- Return delayed message 0, 1, .... x seconds delayed to implement primitive throttling
    return [DMessage t $ IRC.joinChan msg | (msg,t) <- zip chunks [0, 1..] ]
