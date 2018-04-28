{-# LANGUAGE OverloadedStrings #-}
module Karmator.Config
    ( simpleConfig
    ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString, toString)


--
-- Notes: Design stuff for the config management module
--
--  * This is mainly for the bot interface/lookup/plugin config
--  * I don't think it will have config-file bits (initially), it only
--      focuses on config management (trees of config) and providing it to
--      plugins and the bot itself.
--
-- For config it would be a bit tricky maybe with pure haskell and config
-- files but with a sqldb it would be fairly natural i would think.
--
-- Basically we have sort of a tree config from the most general to the
-- most specific, IE.
--
-- Global -> Network -> Channel -> User in which each level is more
-- specific than the previous level, so for ex an config value that has
-- something set for a specific user would only apply for that specific
-- user and we would use a different one for anyone else.
--
-- The trick is when we look up the config value, we look up the most
-- specific version first, and if we can't find that, we keep on iterating
-- till we find the next most specific config or a default value. All
-- config must have a default of some form (I think).
--
-- Don't need to have a value set for a config in between the tiers, ie you
-- can set a global config, then set a new value for that config for
-- a specific user and done.
--
-- Now the main issue is probably going to be developing some form of
-- custom config file format that can handle this tiering, and in theory
-- the ini file format should be general enough if we can do the tier
-- selectors in the [section] headers for each group of config maybe. It'll
-- be a bit on the flat side which is unfortunate.
--
-- One possible approach for handling the global and bot/network bits as
-- just another set of plugins, ie named 'Global' plugin and 'Network'
-- plugin so that we can treat the infrastructure/lookup like any other
-- plugins possibly, could look into weechat config format/infra for
-- inspiration.  Here's an example config selectof format that we could
-- use.
--
-- 'Bot.config_key'
-- 'Network:dakko.config_key'
-- 'Network:dakko.Channel:#foobar.config_key'
--
-- One trick that may remain is figuring out how to handle config that does
-- not make sense to have higher tier of config, ie some config are global
-- only, or network only and makes zero sense to be able to override at the
-- channel/user level.
--

simpleConfig = putStrLn "Lol"

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
