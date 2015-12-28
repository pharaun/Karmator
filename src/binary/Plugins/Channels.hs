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


joinMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!join")
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
                (case Set.null channelSet of
                    True  -> []
                    False -> [CMessage $ IRC.privmsg (whichChannel m) $ fromString ("Joining: " ++ show (map toString $ Set.toList channelSet))]
                ) ++
                (case Set.null ignoredSet of
                    True  -> []
                    False -> [CMessage $ IRC.privmsg (whichChannel m) $ fromString ("Blacklisted: " ++ show (map toString $ Set.toList ignoredSet))]
                ) ++
                [DMessage t $ IRC.joinChan msg | (msg,t) <- zip chunks [0, 1..] ]
                )

joinJoin _ _ _ _ = return []


--
-- Part & Kick
--
kickMatch = exactCommand "KICK"
partMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!part")

-- TODO: part works if you are in that said channel, can't give a '!part #chan' like '!join #chan'
-- TODO: do we want to prevent !part from working for a "mandatory" channels (in the config)?
kickPartLeave network mm@(EMessage _ m) = do
    let channel = head $ IRC.msg_params m

    pool <- ask
    liftIO $ flip runSqlPool pool $ do
        c <- modifyState moduleKey readSet showSerialize (joinKey network) (Set.delete (toString channel))
        case c of
            -- TODO: This doesn't work quite right, because even when its
            -- a empty set it still returns "something" thus the key never gets
            -- deleted
            Nothing -> deleteState moduleKey (joinKey network)
            Just _  -> return ()

    return [CMessage $ IRC.part (whichChannel mm)]
kickPartLeave _ _ = return []


--
-- List of channels to join
--
listMatch = liftM2 (&&) (exactCommand "PRIVMSG") (commandMessage "!list")
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

    let chanList = case chan of
            Nothing -> cs
            -- Subset of channels that aren't on the blacklist
            Just x  -> cs ++ Set.toList (Set.map fromString x `Set.difference` chanBlacklist)

    let chunks = map (BS.intercalate ",") $ chunksOf maxJoin chanList

    -- Return delayed message 0, 1, .... x seconds delayed to implement primitive throttling
    return [DMessage t $ IRC.joinChan msg | (msg,t) <- zip chunks [0, 1..] ]
