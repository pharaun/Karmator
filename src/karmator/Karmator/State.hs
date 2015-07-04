{-# LANGUAGE GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, DeriveGeneric, FlexibleInstances #-}
module Karmator.State
    ( SimpleState(..)
    , migrateSimpleState
    , showSerialize
    , readDeserialize
    , getState
    , setState
    , modifyState
    ) where

import Control.Monad.Except
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString, toString)

-- Sql stuff
import qualified Database.Persist.TH as P
import Database.Persist.Sql

--
-- Notes: Interface stuff
--
-- There is three tier of stored state:
--  1) SimpleState - Key-Value storage
--  2) SingleTable - Custom table schema exported to plugin (TBD)
--  3) CustomSql - Single sqlite file exported to plugin (TBD) (Karma like)
--
--  This document is currently concerned about tier 1 (SimpleState).
--
--  * moduleName (generated via TH) (TODO: TH generate a newtype?)
--  * stateKey (plugin picks/uses its own key)
--  * stateType (The type of the state - ie for serialization/deserialization) (Or alt we can delegate that to the plugin?)
--  * state - a ByteString Blob (whatever the plugin needs for state is stored here)
--

--
-- Shared Generic Data Schema
--
P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateSimpleState"] [P.persistLowerCase|
SimpleState
    moduleName  Text
    stateKey    Text
    state       ByteString
    Primary moduleName stateKey
    deriving Show Eq
|]

-- TODO: sum-type (maybe a persist ADT)
data StateType = ShowRead | Json
    deriving (Show, Read, Eq)


showSerialize :: Show a => a -> ByteString
showSerialize = fromString . show

readDeserialize :: Read a => ByteString -> a -- TODO: should be (Maybe a)
readDeserialize = read . toString

getState :: (MonadIO m) => Text -> (ByteString -> a) -> Text -> SqlPersistT m (Maybe a)
getState m d k = either
    (\_  -> return Nothing)
    (\k' -> return . maybe Nothing (\(SimpleState _ _ s) -> Just $ d s) =<< get k')
    (keyFromValues [PersistText m, PersistText k])

-- This overwrites the already set state
setState :: (MonadIO m) => Text -> (a -> ByteString) -> Text -> a -> SqlPersistT m ()
setState m d k v = either
    (\_  -> return ())
    (\k' -> do
        -- TODO: workaround till `repsert` and by proxy `insertKey` is fixed
        a <- get (k' :: Key SimpleState)
        case a of
            Nothing -> insert_ (SimpleState m k $ d v)
            Just _  -> replace k' (SimpleState m k $ d v)
        )
    (keyFromValues [PersistText m, PersistText k])

-- TODO: clean up the case with a maybe
modifyState :: (MonadIO m) => Text -> (ByteString -> a) -> (b -> ByteString) -> Text -> (a -> b) -> SqlPersistT m (Maybe b)
modifyState m d s k f = do
    a <- getState m d k
    case a of
        Nothing -> return Nothing
        Just a' -> do
            let b = f a'
            setState m s k b
            return $ Just b

-- TODO: needs to have a delete


-- TODO: Do we want to always do a query upon each get/set/modify or do we
-- want to pre-fetch all related entries and put it into a cached state and
-- then update after the plugin is ran?
--
-- TODO: It should be possible to hide/bind the plugin name and maybe even
-- the (de)serialization functions and then the user only needs to present
-- the key/value
--
-- Also we probably want to find a way to separate persist from
-- non-persistent state (probably just via api access) (ie persistent state
-- is stored/readen by this api
--
-- Also probably useful to provide some set of highlevel stuff like..
-- "updates, set, delete, etc"
--  - https://hackage.haskell.org/package/ircbot-0.6.4/docs/src/Network-IRC-Bot-BotMonad.html#BotPartT
--  - Or a freeT Monad for a nicer AST/interface to handle various bits.




--
-- NOTES: weechat config file format
--  * hooks on file change/section change/config change
--
-- irc.conf  <- plugin filename config
--
-- [section]
--
-- [section]
-- config = foo
-- config.something = bar
--
--
-- config:
--  - type (boolean/int/string/color/types
--  - description
--  - default
--  - allowed string values (enum)
--  - min/max for integer
--  - null value allowed
--  - validation hook
