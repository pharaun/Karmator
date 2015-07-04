{-# LANGUAGE OverloadedStrings #-}
module Plugins.Channels
    ( sqlWrapper

    , inviteMatch
    , inviteJoin

    , kickMatch
    , kickLeave

    , partMatch
    , partLeave

    , listMatch
    , listChannel
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Database.Persist.Sql
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString, toString)

import Karmator.State
import Karmator.Types
import Karmator.Filter
import qualified Network.IRC as IRC

--
-- Test the wrapper stuff so i don't need to modify all over
--
sqlWrapper :: MonadIO m => (BotEvent -> ReaderT ConnectionPool m (Maybe BotCommand)) -> ConnectionPool -> BotEvent -> m (Maybe BotCommand)
sqlWrapper c pool e = runReaderT (c e) pool



--modifyState :: (MonadIO m) => Text -> (ByteString -> a) -> (b -> ByteString) -> Text -> (a -> b) -> SqlPersistT m (Maybe b)
--showSerialize :: Show a => a -> ByteString
--readDeserialize :: Read a => ByteString -> a -- TODO: should be (Maybe a)


--
-- Invite
--
inviteMatch :: BotEvent -> Bool
inviteMatch             = exactCommand "INVITE"

inviteJoin :: MonadIO m => BotEvent -> ReaderT ConnectionPool m (Maybe BotCommand)
inviteJoin (EMessage _ m) = do
    let channel = head $ tail $ IRC.msg_params m

    pool <- ask
    liftIO $ flip runSqlPool pool (do
        c <- modifyState "Plugins.Channels" readDeserialize showSerialize "test.auto_join_channel" (\a -> Set.insert (toString channel) a)
        case c of
            Nothing -> setState "Plugins.Channels" showSerialize "test.auto_join_channel" (Set.singleton $ toString channel)
            Just x  -> return ()
        )

    return $ Just $ CMessage $ IRC.joinChan channel


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
            Just x  -> return ()
        )
    return Nothing

partMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!part")
partLeave (EMessage _ m) = do
    let channel = head $ IRC.msg_params m

    pool <- ask
    liftIO $ flip runSqlPool pool (do
        c <- modifyState "Plugins.Channels" readDeserialize showSerialize "test.auto_join_channel" (\a -> Set.delete (toString channel) a)
        case c of
            Nothing -> return () -- deleteState "Plugins.Channels" "test.auto_join_channel"
            Just x  -> return ()
        )

    return $ Just $ CMessage $ IRC.part channel


--
-- List of channels to join
--
listMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!list")
listChannel m = do
    pool <- ask
    chan <- liftIO $ flip runSqlPool pool (getState "Plugins.Channels" readDeserialize "test.auto_join_channel")

    return $ Just $ CMessage $ IRC.privmsg (whichChannel m) (fromString $ show (chan :: Maybe (Set String)))
