{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text
import Data.Time.LocalTime
import Database.Persist
import Database.Persist.Sql

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

-- Typeclass stuff
instance PersistField LocalTime where
    toPersistValue lt@(LocalTime _ _) = PersistDbSpecific $ C8.pack $ show lt

    -- TODO: This will properly load the localtime from the database, but
    -- we will unfortunately be stuck with "PDT and/or PST" so need to code
    -- up a function to perform the conversion properly to UTC
    fromPersistValue (PersistDbSpecific t) = Right $ read $ C8.unpack t :: Either Text LocalTime
    fromPersistValue (PersistByteString t) = Right $ read $ C8.unpack t :: Either Text LocalTime
    fromPersistValue (PersistText t)       = Right $ read $ T.unpack t :: Either Text LocalTime
    fromPersistValue _                     = Left "LocalTime values must be converted from PersistDbSpecific"

instance PersistFieldSql LocalTime where
    sqlType _    = SqlOther "DATETIME"
