{-# LANGUAGE Rank2Types, DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction #-}
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

type Msg = String

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

deriving instance Typeable ClockTime
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

--    { init  :: (Monad f, MonadIO f) => f [Dynamic]
--    , match :: [Dynamic] -> Msg -> Bool
--    , eval  :: (Monad f, MonadIO f) => [Dynamic] -> Msg -> f (Maybe Msg)




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

