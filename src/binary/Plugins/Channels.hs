{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Plugins.Channels
    ( inviteMatch
    , inviteJoin

    , joinMatch
    , joinJoin

    , kickMatch
    , partMatch
    , kickPartLeave

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

import Text.Parsec hiding (getState, modifyState, setState)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Karmator.State
import Karmator.Types
import Karmator.Filter
import qualified Network.IRC as IRC

-- TODO: temp
import Plugins.Karma.Karma (chanParse)


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


-- Specify the type of readDeserialize
readSet :: Read (Set String) => B.ByteString -> Set String
readSet = readDeserialize

-- Key
joinKey network = T.concat [ T.pack network, ".auto_join_channel" ]
moduleKey = "Plugins.Channels"


--
-- Invite
--
inviteMatch :: BotEvent -> Bool
inviteMatch             = exactCommand "INVITE"

inviteJoin :: MonadIO m => String -> Set B.ByteString -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
inviteJoin network chanBlacklist mm@(EMessage _ m) = do
    let channel = head $ tail $ IRC.msg_params m

    if Set.member channel chanBlacklist
    then return [CMessage $ IRC.privmsg (whichChannel mm) "Channel on blacklist"]
    else do
        pool <- ask
        liftIO $ flip runSqlPool pool $ do
            c <- modifyState moduleKey readSet showSerialize (joinKey network) (Set.insert (toString channel))
            case c of
                Nothing -> setState moduleKey showSerialize (joinKey network) (Set.singleton $ toString channel)
                Just _  -> return ()

        return [CMessage $ IRC.joinChan channel]

inviteJoin _ _ _ = return []


joinMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!join")
joinJoin  network chanBlacklist maxJoin m@(EMessage _ _) =
    case parse chanParse "(irc)" $ T.decodeUtf8 $ messageContent m of
        (Left _)        -> return [CMessage $ IRC.privmsg (whichChannel m) "Channel parse failed"]
        (Right [])      -> return [CMessage $ IRC.privmsg (whichChannel m) "Please specify a channel to join"]
        (Right channel) -> do
            -- Subset of channels that aren't on the blacklist
            let channelSet = Set.fromList (map (fromString . T.unpack) channel) `Set.difference` chanBlacklist
            let ignoredSet = Set.fromList (map (fromString . T.unpack) channel) `Set.intersection` chanBlacklist

            unless (Set.null channelSet) $ do
                pool <- ask
                liftIO $ flip runSqlPool pool $ do
                    c <- modifyState moduleKey readSet showSerialize (joinKey network) (
                        \a -> Set.union a (Set.map toString channelSet)
                        )
                    case c of
                        Nothing -> setState moduleKey showSerialize (joinKey network) (Set.map toString channelSet)
                        Just _  -> return ()


            -- Actually join these channels
            let chunks = map (BS.intercalate ",") $ chunksOf maxJoin (Set.toList channelSet)

            -- Return delayed message 0, 1, .... x seconds delayed to implement primitive throttling
            return (
                [CMessage $ IRC.privmsg (whichChannel m) $ fromString ("Joining: " ++ show (map toString $ Set.toList channelSet))] ++
                [CMessage $ IRC.privmsg (whichChannel m) $ fromString ("Blacklisted: " ++ show (map toString $ Set.toList ignoredSet))] ++
                [DMessage t $ IRC.joinChan msg | (msg,t) <- zip chunks [0, 1..] ]
                )

joinJoin _ _ _ _ = return []


--
-- Part & Kick
--
kickMatch = exactCommand "KICK"
partMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!part")

kickPartLeave network (EMessage _ m) = do
    let channel = head $ IRC.msg_params m

    pool <- ask
    liftIO $ flip runSqlPool pool $ do
        c <- modifyState moduleKey readSet showSerialize (joinKey network) (Set.delete (toString channel))
        case c of
            Nothing -> return () -- deleteState moduleKey (joinKey network)
            Just _  -> return ()

    return []
kickPartLeave _ _ = return []


--
-- List of channels to join
--
listMatch = liftM2 (&&) (exactCommand "PRIVMSG") (prefixMessage "!list")
listChannel network m = do
    pool <- ask
    chan <- liftIO $ runSqlPool (getState moduleKey readSet (joinKey network)) pool

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
motdMatch = exactCommand "004"

motdJoin :: MonadIO m => String -> [B.ByteString] -> Set B.ByteString -> Int -> BotEvent -> ReaderT ConnectionPool m [BotCommand]
motdJoin network cs chanBlacklist maxJoin _ = do
    pool <- ask
    chan <- liftIO $ runSqlPool (getState moduleKey readSet (joinKey network)) pool

    case chan of
        Nothing -> return []
        Just x  -> do
            -- Subset of channels that aren't on the blacklist
            let channelSet = Set.map fromString x `Set.difference` chanBlacklist

            let chunks = map (BS.intercalate ",") $ chunksOf maxJoin (cs ++ Set.toList channelSet)

            -- Return delayed message 0, 1, .... x seconds delayed to implement primitive throttling
            return [DMessage t $ IRC.joinChan msg | (msg,t) <- zip chunks [0, 1..] ]
