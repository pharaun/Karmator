{-# LANGUAGE GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, DeriveGeneric, FlexibleInstances #-}
module Karmator.State
    ( test
    ) where

import Control.Monad.Except
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString, toString)

-- Sql stuff
import qualified Database.Persist.TH as P
import Database.Persist.Sql
import Database.Persist.Sqlite hiding (get)

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

--getState :: Read a => Text -> (ByteString -> a) -> Text -> Maybe a
getState m d k = do
    let k' = keyFromValues [PersistText m, PersistText k]
    case k' of
        Left _    -> return Nothing
        Right k'' -> do
            a <- get (k'' :: Key SimpleState)
            case a of
                Nothing -> return Nothing
                Just (SimpleState _ _ s) -> return $ Just $ d s

--setState :: Show a => Text -> (a -> ByteString) -> Text -> a -> ()
setState m d k v = insert_ $ SimpleState m k $ d v


test :: IO ()
test = runSqlite ":memory:" $ do
    runMigration migrateSimpleState

    setState "a" showSerialize "b" "c"
    setState "a" showSerialize "c" "d"

    a <- getState "a" readDeserialize "b"
    b <- getState "a" readDeserialize "c"

    liftIO $ print (a :: Maybe String)
    liftIO $ print (b :: Maybe String)


--  a <- getState deserialize '(moduleName) "channelList"           <- Maybe a ?
--  setState serialize '(moduleName) "channelList" "foo,bar,baz"
--
-- Do we want to wrap these state stuff into StateT and let plugin
-- access/write that way? or do we just want to expose it as a plain
-- highlevel api?
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
