{-# LANGUAGE OverloadedStrings #-}
module Plugins.Karma.Types
    ( KarmaReply(..)
    , KarmaType(..)
    , PartialKarmaType(..)
    , Karma(..)
    , KarmaCandidates(..)
    , Config(..)
    ) where

-- String
import qualified Data.Text as T

data KarmaReply = KarmaReply T.Text (Maybe [Karma])
    deriving (Show)

data KarmaType = Upvote | Downvote | Sidevote
    deriving (Show, Read, Eq)

data PartialKarmaType = Up | Down | Side
    deriving (Show, Read, Eq)

-- Final karma results
data Karma = Karma
    { kType :: KarmaType
    , kMessage :: T.Text
    } deriving (Show, Eq)

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
