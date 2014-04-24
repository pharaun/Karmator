{-# LANGUAGE Rank2Types, DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction, OverloadedStrings, TemplateHaskell #-}
import Data.Dynamic
import Data.Typeable

import Data.List hiding (init)
import System.IO
import System.Time
import Control.Monad
import Control.Monad.IO.Class
import Prelude hiding (log, init)
import Data.Maybe
import Control.Applicative

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Control.Monad.Trans.State.Strict

-- TEMPLATE HASKELL
import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Syntax

moduleName = fmap loc_module qLocation >>= \mod -> return (AppE (VarE (mkName "Data.ByteString.Char8.pack")) (LitE (StringL mod)))
-- TEMPLATE HASKELL

type Msg = String
deriving instance Typeable ClockTime

data PluginPlus = PluginPlus
    -- Plugin Name
    { nameP :: B.ByteString

    -- Either initalize or loads a persisted state
    , initP :: (Monad f, MonadIO f) => Maybe B.ByteString -> f Dynamic

    -- Saves the persist state
    , saveP :: Dynamic -> B.ByteString

    -- Does the message match this plugin
    , matchP :: Msg -> State Dynamic Bool

    -- Evalulate the message and optionally return a result
    , evalP :: (Monad f, MonadIO f) => Msg -> StateT Dynamic f (Maybe Msg)
    }

-- Test implementation
pingP = PluginPlus pingName pingInit pingSave pingMatch pingEval
    where
        pingName    = "Ping"
        pingInit _  = return $ toDyn ()
        pingSave _  = B.empty
        pingMatch m = return $ m == "Hi"
        pingEval m  = return $ Just m

uptimeP = PluginPlus uptimeName uptimeInit uptimeSave uptimeMatch uptimeEval
    where
        uptimeName    = "Uptime"
        uptimeInit _  = liftM toDyn (liftIO getClockTime)
        uptimeSave _  = B.empty
        uptimeMatch m = return $ m == "Bye"
        uptimeEval m  = do
            now  <- liftIO getClockTime
            past <- get

            return $ case fromDynamic past of
                Nothing -> Nothing
                Just p  -> Just $ pretty $ diffClockTimes now p


-- Supporting code for running and managing the state
pluginP :: [PluginPlus]
pluginP = [pingP, uptimeP]

-- TODO: Probably want to have sanity check or a way to enforce nameP to be unique
initEmptyPluginData :: (Functor f, Monad f, MonadIO f) => [PluginPlus] -> f (Map.Map B.ByteString Dynamic)
initEmptyPluginData l = liftM Map.fromList $ zip (map nameP l) `fmap` mapM (`initP` Nothing) l


commandP :: (Monad f, MonadIO f)
         => [PluginPlus]
         -> Map.Map B.ByteString Dynamic
         -> Msg
         -> f (Maybe Msg, Map.Map B.ByteString Dynamic)
commandP s m = undefined
--
-- Need a neat way to be able to take a list of match rule then upon the
-- first or whatever match, pass it on to the eval to evalulate it
--
-- I wonder if there's no neat way to just iterate through the plugin list
-- then run the match and then if it matches then eval then keep going... ?
--



--    -- Does the message match this plugin
--    , matchP :: Msg -> State Dynamic Bool
--
--    -- Evalulate the message and optionally return a result
--    , evalP :: (Monad f, MonadIO f) => Msg -> StateT Dynamic f (Maybe Msg)
--    -- TODO: look into seeing if there's a way to setup some form of
--    -- generic filtering rules such as "if chan = y send to x", etc..
--    -- Perhaps Pipe.Prelude.Filter
--    result <- catMaybes <$> forM
--        [ \a -> return $ if pingMatch a then ping a else Nothing
--        , \a -> return $ if motdMatch a then motdJoin a else Nothing
--        , \a -> if uptimeMatch a then uptime t a else return $ Nothing -- IO
--        ]
--        (\a -> a msg)
--
--    -- TODO: Unsafe head
--    unless (null result) (yield $ head result)


plugins = [ping, uptime]

initPlugins :: (Functor f, Monad f, MonadIO f) => f [Dynamic]
initPlugins = fmap concat $ mapM init plugins

matchPlugins :: (Monad f, MonadIO f)
             => [([Dynamic] -> Msg -> Bool, [Dynamic] -> Msg -> f (Maybe Msg))]
matchPlugins = fmap (\a -> (match a, eval a)) plugins

command :: (Monad f, MonadIO f)
        => [([Dynamic] -> Msg -> Bool, [Dynamic] -> Msg -> f (Maybe Msg))]
        -> [Dynamic]
        -> Msg
        -> f [Maybe Msg]
command match dat msg = forM match (\(a, b) -> if a dat msg then b dat msg else (return Nothing))


runCommand init msg = do
    command matchPlugins init msg






-- Types
data Plugin' = Plugin'
    { init  :: (Monad f, MonadIO f) => f [Dynamic]
    , match :: [Dynamic] -> Msg -> Bool
    , eval  :: (Monad f, MonadIO f) => [Dynamic] -> Msg -> f (Maybe Msg)
    }

-- getDynamic - return first matching one
getDynamic :: (Typeable a) => [Dynamic] -> Maybe a
getDynamic = listToMaybe . catMaybes . map fromDynamic

ping = Plugin' pingInit pingMatch pingMsg
    where
        pingInit :: Monad f => f [Dynamic]
        pingInit = return []
        pingMatch _ m = m == "Hi"
        pingMsg _ m = return $ Just m

uptime = Plugin' uptimeInit uptimeMatch uptimeMsg
    where
        uptimeInit :: MonadIO f => f [Dynamic]
        uptimeInit = do
            t <- liftIO $ getClockTime
            return [toDyn t]

        uptimeMatch _ m = m == "Bye"

        uptimeMsg :: MonadIO f => [Dynamic] -> Msg -> f (Maybe Msg)
        uptimeMsg d m = do
            now <- liftIO $ getClockTime
            return $ case getDynamic d of
                Nothing   -> Nothing
                Just past -> do
                    Just $ pretty $ diffClockTimes now past



--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s

