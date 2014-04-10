{-# LANGUAGE OverloadedStrings #-}
module Plugins.Ping
    ( pingMatch
    , ping

    , motdMatch
    , motdJoin
    ) where


import Karmator.Filter
import qualified Network.IRC as IRC

--
-- TODO:
--  * Regular plugin interface
--      - Handle/filter - a way to specify exactly what the plugin is interested in listening for
--      - Init - A way to initalize any state/stored state
--      - config/state - backing store of some form for configuration store and state store if possible
--      - handler - A way to match on a filter rule then invoke the plugin to handle it
--      - shutdown - A way to gracefully shut down the plugin and store any config/state as needed
--
--  * AUTH plugin
--      - Should be able to provide plugin for handling auth because there's several ways to auth to a network
--
--  * Additional
--      - Some form of state backing store, probably sqlite of some form, should be able to key in new config/state to be persisted
--          to disk ala xmonad extensiable state model
--      - Should be able to in theory have multiple plugins running independently and the plugin should not care how its being ran
--      - Should be a way to support separate plugin that can maybe use the same shared backing store (ala karmator)


--
-- Ping
--
pingMatch :: IRC.Message -> Bool
pingMatch = exactCommand "PING"

-- TODO: Unsafe head
ping :: IRC.Message -> Maybe IRC.Message
ping = Just . IRC.pong . head . IRC.msg_params


--
-- Motd Join
-- TODO: improve this (Such as list of channels to join)
--
motdMatch  = exactCommand "700"
motdJoin _ = Just $ IRC.joinChan "#test"


--
-- Uptime
-- TODO: Improve state persisting
--


--
---- TODO: extend the IRC.privmsg to support sending to multiple people/channels
--uptime :: MonadIO m => ClockTime -> IRC.Message -> m (Maybe IRC.Message)
--uptime t msg = do
--    now <- liftIO $ getClockTime
--
--    return $ if "PRIVMSG" /= IRC.msg_command msg
--    then Nothing
--    else if "!uptime" `BS.isPrefixOf` (head $ tail $ IRC.msg_params msg) -- TODO: unsafe head/tail
--         then Just $ IRC.privmsg "#test" (C8.pack $ pretty $ diffClockTimes now t)
--         else Nothing
--
----
---- Pretty print the date in '1d 9h 9m 17s' format
----
--pretty :: TimeDiff -> String
--pretty td = join . intersperse " " . filter (not . null) . map f $
--    [(years          ,"y") ,(months `mod` 12,"m")
--    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
--    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
--  where
--    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
--    hours   = mins   `div` 60 ; days   = hours  `div` 24
--    months  = days   `div` 28 ; years  = months `div` 12
--    f (i,s) | i == 0    = []
--            | otherwise = show i ++ s






----
---- Handshake for the initial connection to the network
----
--handshake :: Monad m => ServerState -> Producer IRC.Message m ()
--handshake ss = do
--    let sc  = config ss
--    let sps = session ss
--
--    let nick = head $ nicks sc -- TODO: unsafe head
--    let chan = head $ channels sps -- TODO: unsafe head
--    let user = userName sc
--    let pasw = serverPassword sc
--
--    -- Required for establishing a connection to irc
--    case pasw of
--        Just x  -> yield $ pass x
--        Nothing -> return ()
--    yield $ IRC.nick nick
--    yield $ IRC.user nick "0" "*" user
--
--    -- TODO: add support for "auth ping/pong" before registering/joining channels
--
--    -- Setup the channels
--    yield $ IRC.joinChan chan
--
--    return ()
--
----
---- take the inbound message, loop it through a list of
---- functions/pipe/whatever
----
---- Then for early exit take the first one that response and send it on
---- downstream via yield
----
---- TODO: implement a form or precidate logic such as "and [privmsg, or [ user "xyz", server "xy" ] ] -> command
---- TODO: implement some form of infrastructure for state tracking for stateful stuff like "invite to new channel, want to stay there, leave, etc...
----
--command :: (Monad m, MonadIO m) => ClockTime -> Pipe IRC.Message IRC.Message m ()
--command t = forever $ do
--    msg <- await
--
--    -- TODO: look into seeing if there's a way to setup some form of
--    -- generic filtering rules such as "if chan = y send to x", etc..
--    -- Perhaps Pipe.Prelude.Filter
--    result <- catMaybes <$> forM
--        [ return . ping
--        , uptime t
--        , return . motdJoin
----        , quit -- TODO: need to implement a way to exit/die so that we gracefully exit from the server
--        ]
--        (\a -> a msg)
--
--    -- TODO: Unsafe head
--    unless (null result) (yield $ head result)
