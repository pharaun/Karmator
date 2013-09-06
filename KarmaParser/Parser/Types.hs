{-# LANGUAGE OverloadedStrings #-}
module Parser.Types
    ( IrcMessage(..)
    , KarmaReply(..)
    , KarmaType(..)
    , PartialKarmaType(..)
    , Karma(..)
    , KarmaCandidates(..)
    , Config(..)
    ) where

-- Json parsing
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

-- String
import qualified Data.Text as T


data IrcMessage = IrcMessage
    { ircNick :: T.Text
    , ircMessage :: T.Text
    } deriving (Show)

instance FromJSON IrcMessage where
    parseJSON (Object v) =
        IrcMessage
        <$> (v .: "nick")
        <*> (v .: "message")
    parseJSON _ = mzero


data KarmaReply = KarmaReply
    { rNick :: Maybe T.Text
    , rKarma :: Maybe [Karma]
    , rError :: Maybe String
    } deriving (Show)

instance ToJSON KarmaReply where
    toJSON p = object
        [ "nick"    .= rNick p
        , "karma"   .= rKarma p
        , "error"   .= rError p
        ]


data KarmaType = Upvote | Downvote | Sidevote
    deriving (Show, Read, Eq)

data PartialKarmaType = Up | Down | Side
    deriving (Show, Read, Eq)


-- Final karma results
data Karma = Karma
    { kType :: KarmaType
    , kMessage :: T.Text
    } deriving (Show, Eq)

instance ToJSON Karma where
    toJSON p = object
        [ "karma_type"  .= (dumpKarmaType $ kType p)
        , "message"     .= kMessage p
        ]

dumpKarmaType :: KarmaType -> Value
dumpKarmaType Upvote = String "Upvote"
dumpKarmaType Downvote = String "Downvote"
dumpKarmaType Sidevote = String "Sidevote"


-- Karma candidates from parsing
data KarmaCandidates = KarmaCandidate
    { kcMessage :: String
    , kcKarma :: String
    }
                     | KarmaNonCandidate
    { kncMessage :: String
    }
    deriving (Show, Eq)


-- Config stuff
data Config = Config
    { strictMatchList :: [T.Text]
    , prefixMatchList :: [T.Text]

    , partialKarma :: [ (Char, PartialKarmaType) ]
    , totalKarma :: [ (Char, KarmaType) ]

    , openBrace :: Char
    , closeBrace :: Char

    } deriving (Show)
