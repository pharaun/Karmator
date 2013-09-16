{-# LANGUAGE OverloadedStrings #-}
import System.IO (stdin, stdout, stderr, hSetBuffering, BufferMode(..), hPrint, hIsEOF, Handle, hSetEncoding, utf8)
import Control.Monad (mzero, unless, liftM)

-- Json parsing
import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson

import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.List as DL

-- Parsec
import Text.Parsec

-- Karma module
import Parser.Karma
import Parser.Types

-- Configuration
import Data.ConfigFile
import System.IO.UTF8 (readFile)
import Prelude hiding (readFile)
import System.Environment.UTF8 (getArgs)


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    -- Force the encoding to utf8, this is only communicating with a wrapping program which
    -- emits and digests strings in utf8 format.
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8

    configFile <- parseArgs `fmap` getArgs

    case configFile of
        Just x  -> do
            config <- getConfig x
            parsingLoop stdin stdout config
        Nothing -> putStrLn "Please provide one parser config file."

    where
        parseArgs :: [String] -> Maybe String
        parseArgs [x] = Just x
        parseArgs _   = Nothing

        parsingLoop :: Handle -> Handle -> Config -> IO ()
        parsingLoop i o c = do
            done <- hIsEOF i
            unless done $ (sendReply o . parseInput c =<< B.hGetLine i) >> parsingLoop i o c

        parseInput :: Config -> B.ByteString -> KarmaReply
        parseInput config input = do
            -- Aeson process the json
            let parsed = eitherDecode (BL.fromChunks [input]) :: Either String IrcMessage

            case parsed of
                (Left e)  -> KarmaReply Nothing Nothing (Just e)
                (Right j) ->
                    if filterBot config $ ircNick j
                    then KarmaReply (Just $ ircNick j) Nothing Nothing
                    else do
                        -- Clean up the nick
                        let nick = case parse nickDeFuzzifier "(stdin)" $ ircNick j of
                                (Left _)  -> ircNick j
                                (Right n) -> n

                        -- Parse the message
                        let karma = parse (karmaParse config) "(stdin)" $ ircMessage j

                        case karma of
                            (Left _)  -> KarmaReply (Just nick) Nothing Nothing -- Failed to find a karma entry
                            (Right k) -> do
                                -- Scan the extracted karma expression for the nick
                                let filteredKarma = filterKarma [nick, ircNick j] k

                                KarmaReply (Just nick) (Just filteredKarma) Nothing

sendReply :: Handle -> KarmaReply -> IO ()
sendReply o r = B.hPut o $ B.concat $ (BL.toChunks $ encode r) ++ [fromString "\n"]

filterKarma :: [T.Text] -> [Karma] -> [Karma]
filterKarma n = filter (\Karma{kMessage=msg} -> msg `DL.notElem` n)

-- Load the config
getConfig :: FilePath -> IO Config
getConfig conf = do
    contents <- readFile conf

    let config = do
        c <- readstring emptyCP contents

        strictMatch <- get c "nick_filtering" "strict_match"
        prefixMatch <- get c "nick_filtering" "prefix_match"

        -- Force the Parser to invoke Read on the Partial/KarmaTypes
        partialKarma <- get c "karma_parsing" "partial" :: Either CPError [ (Char, PartialKarmaType) ]
        totalKarma <- get c "karma_parsing" "total" :: Either CPError [ (Char, KarmaType) ]

        -- Braces
        open <- get c "karma_parsing" "open_brace"
        close <- get c "karma_parsing" "close_brace"

        return $ Config strictMatch prefixMatch partialKarma totalKarma open close

    case config of
        Left cperr   -> error $ show cperr
        Right config -> return config
