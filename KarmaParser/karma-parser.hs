{-# LANGUAGE OverloadedStrings #-}
import System.IO (stdin, stdout, stderr, hSetBuffering, BufferMode(..), hPrint, hIsEOF, Handle)
import Control.Monad (mzero, unless, liftM)

-- Json parsing
import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson

-- TODO: Make it utf8 sensitive (aeson + UTF8.string, etc)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.List as DL

-- Parsec
import Text.Parsec

-- Karma module
import Parser.Karma
import Parser.Types

-- Configuration
import Data.ConfigFile


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    config <- getConfig "KarmaParser/parser.cfg"
    parsingLoop stdin stdout config

    where
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
                    if (filterBot config) $ ircNick j
                    then KarmaReply (Just $ ircNick j) Nothing Nothing
                    else do
                        -- Clean up the nick
                        let nick = case (parse nickDeFuzzifier "(stdin)" $ ircNick j) of
                                (Left _)  -> ircNick j
                                (Right n) -> n

                        -- Parse the message
                        let karma = parse (karmaParse config) "(stdin)" $ ircMessage j

                        case karma of
                            (Left _)  -> KarmaReply (Just nick) Nothing Nothing -- Failed to find a karma entry
                            (Right k) -> do
                                -- Scan the extracted karma expression for the nick
                                let filteredKarma = liftM (filterKarma [nick, ircNick j]) k

                                KarmaReply (Just nick) filteredKarma Nothing

sendReply :: Handle -> KarmaReply -> IO ()
sendReply o = C8.hPutStrLn o . B.concat . BL.toChunks . encode

filterKarma :: [T.Text] -> [Karma] -> [Karma]
filterKarma n k = filter (\Karma{kMessage=msg} -> msg `DL.notElem` n) k

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
