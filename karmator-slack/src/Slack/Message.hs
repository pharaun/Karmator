{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Slack.Message
    ( Message(..)

    -- Slack user id mapper
    , SlackMap
    , newSlackMap
    , initFromSession
    , getUserId
    , getUserName

    -- Slack message parser
    , remapMessage
    , remapAtHere
    ) where

import System.IO (Handle)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO, writeTVar)
import Control.Error (runExceptT)
import Control.Monad.Reader (foldM, MonadIO, liftIO)
import Data.Typeable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Slack Parser
import qualified Web.Slack.Types.Time as WSTT
import qualified Web.Slack.WebAPI as WSA
import qualified Web.Slack as WS
import qualified Network.Wreq as W
import Control.Lens ((.~))
import qualified Data.Aeson as A

-- For the userid and channel id bits
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM

-- Parsec for remap
import Text.Parsec
import Data.Functor.Identity (Identity)
import Control.Applicative hiding ((<|>), many, optional)
import Data.Maybe

import Karmator.Types

--
-- Slack Message types
--
data Message = Message
    { msg_type :: T.Text -- Message type
    , msg_channel :: Maybe T.Text -- Channel, Private Group, DM
    , msg_cid :: T.Text -- Channel id
    , msg_user :: Maybe T.Text -- User speaking
    , msg_uid :: T.Text -- User id
    , msg_text :: T.Text -- Text content
    , msg_ts :: WSTT.SlackTimeStamp -- Timestamp
    , msg_thread_ts :: Maybe WSTT.SlackTimeStamp
    } deriving (Show)

--
-- Matchers for the Slack instance
--
instance BotEventMatch BS.ByteString Message where
    exactCommand c (EMessage m) = c == (TE.encodeUtf8 $ msg_type m)
    exactCommand _ _            = False

    prefixMessage c (EMessage m) = c `BS.isPrefixOf` (TE.encodeUtf8 $ msg_text m)
    prefixMessage _ _            = False

    commandMessage c (EMessage m) = c == (BS.takeWhile (space /=) $ (TE.encodeUtf8 $ msg_text m))
      where
        space = BS.head " "
    commandMessage _ _            = False

    nickMatch n (EMessage m) = n == (TE.encodeUtf8 $ fromMaybe "" $ msg_user m)
    nickMatch _ _            = False



--
-- Slack Map between username/channel and their ids
--
data SlackMap = SlackMap
    { uidToName :: Map T.Text T.Text
    , nameToUid :: Map T.Text T.Text
--    , cidToChan :: Map T.Text T.Text
--    , chanToCid :: Map T.Text T.Text
    }
    deriving (Show)


newSlackMap :: SlackMap
newSlackMap = SlackMap Map.empty Map.empty


initFromSession :: WS.SlackSession -> TVar SlackMap -> IO ()
initFromSession s m = do
    let uidToName = map getName (WS._slackUsers s)

    atomically $ writeTVar m $ SlackMap (Map.fromList uidToName) (Map.fromList $ map (\(a, b) -> (b, a)) uidToName)
  where
    getName :: WS.User -> (T.Text, T.Text)
    getName u = (WS._getId $ WS._userId u, WS._userName u)


getUserName :: Handle -> WS.SlackHandle -> TVar SlackMap -> T.Text -> IO (Maybe T.Text)
getUserName l h m val = do
    sm <- readTVarIO m
    --let v = Map.lookup val (uidToName sm)
    let v = Map.lookup val (Map.empty)

    case v of
        Just x  -> return x
        -- TODO: Do query
        Nothing -> (do
            res <- runExceptT $ WSA.makeSlackCall (WS.getConfig h) "users.info" (W.param "user" .~ [val])

            case res of
                Left e  -> log l e >> return Nothing
                Right e ->
                    case e of
                        A.Object e' ->
                            case (HM.lookup "user" e') of
                                Nothing  -> log l e' >> return Nothing
                                Just e'' ->
                                    case A.fromJSON e'' :: A.Result WS.User of
                                        A.Success u -> (do
                                            let (id, name) = (WS._getId $ WS._userId u, WS._userName u)

                                            atomically $ modifyTVar' m (\m ->
                                                SlackMap (Map.insert id name $ uidToName m) (Map.insert name id $ nameToUid m)
                                                )

                                            return $ Just name
                                            )
                                        _ -> log l e'' >> return Nothing
                        _ ->  log l e >> return Nothing
            )
  where
    log :: (Show a, Typeable a) => Handle -> a -> IO ()
    log l res = BS.hPutStr l $ BS.concat
        [ "===========\n"
        , "Error Type: "
        , C8.pack $ show $ typeOf res -- TODO: Ascii packing
        , "\n"
        , "Error Message: "
        , C8.pack $ show res -- TODO: Ascii packing
        , "\n"
        , "===========\n"
        ]


getUserId :: WS.SlackHandle -> TVar SlackMap -> T.Text -> IO (Maybe T.Text)
getUserId _ m val = do
    sm <- readTVarIO m
    return $ Map.lookup val (nameToUid sm)


--
-- Remap <@id> to name
--
remapMessage :: MonadIO m => TVar SlackMap -> WS.SlackHandle -> Handle -> T.Text -> m T.Text
remapMessage sm h l msg = do
    case runParser msgParse () "Slack" msg of
        Left e  -> (liftIO $ log l e) >> return msg
        Right p -> foldM process "" p

  where
    process :: MonadIO m => T.Text -> ParsedMsg -> m T.Text
    process str (Txt t)     = return $ T.concat [str, t]
    process str (Ident uid) = (do
        user <- liftIO $ getUserName l h sm uid

        return $ T.concat [str, fromMaybe "invalid" user]
        )

    log :: (Show a, Typeable a) => Handle -> a -> IO ()
    log l res = BS.hPutStr l $ BS.concat
        [ "===========\n"
        , "Error Type: "
        , C8.pack $ show $ typeOf res -- TODO: Ascii packing
        , "\n"
        , "Error Message: "
        , C8.pack $ show res -- TODO: Ascii packing
        , "\n"
        , "===========\n"
        ]


--
-- This block is specifically for breaking up a message into text and ids
--
data ParsedMsg
    = Txt T.Text
    | Ident T.Text
    deriving (Show)

msgParse :: ParsecT T.Text u Identity [ParsedMsg]
msgParse = many1 $ choice [identParse, try txtParse, trashParse]

identParse :: ParsecT T.Text u Identity ParsedMsg
identParse = do
    a <- between (string "<@") (string ">") (many1 $ noneOf ">")
    return $ Ident $ T.pack a

txtParse :: ParsecT T.Text u Identity ParsedMsg
txtParse = do
    a <- anyChar `manyTill` lookAhead (try identParse)
    return $ Txt $ T.pack a

trashParse :: ParsecT T.Text u Identity ParsedMsg
trashParse = do
    a <- many1 anyChar
    return $ Txt $ T.pack a

remapAtHere :: T.Text -> T.Text
remapAtHere x = T.replace "<!here>" "`@here`" $ T.replace "<!channel>" "`@channel`" x
