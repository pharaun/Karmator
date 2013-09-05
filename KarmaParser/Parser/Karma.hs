{-# LANGUAGE OverloadedStrings #-}
module Parser.Karma
    ( KarmaType(..)
    , Karma(..)
    , karmaParse
    , nestedKarmaParse
    , nickDeFuzzifier
    , filterBot
    ) where

-- TODO: Make it utf8 sensitive (aeson + UTF8.string, etc)
import qualified Data.Text as T

-- Parsec
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Functor.Identity (Identity)

import qualified Data.List as L
import Data.Maybe

import Control.Applicative hiding ((<|>), many, optional)

import Data.Traversable (sequenceA)

-- TODO: testing only
import Test.HUnit hiding (test)

import Parser.Types

-- TODO: break this out to the NickName module.
nickDeFuzzifier :: ParsecT T.Text u Identity T.Text
nickDeFuzzifier = do
    many (oneOf "_")
    nick <- choice $ fmap try
        [ manyTill anyToken (oneOf "_-|^[`")
        , manyTill anyToken digit
        , many1 anyToken
        ]
    return $ T.pack nick

-- TODO: Break this out to a nickname module
--  - First search a strictMatchList for a matching nick to ignore
--  - If all else fail, scan a list of prefixes to match to the nick (Simple fuzzy matching)
filterBot :: Config -> T.Text -> Bool
filterBot c nick = if (nick `elem` (strictMatchList c)) then True else (L.any (\a -> T.isPrefixOf a nick) (prefixMatchList c))





-- Entry point of the old karma parser
karmaParse :: Config -> ParsecT T.Text u Identity (Maybe [Karma])
karmaParse _ = do
    msg <- many1 (try (anyToken `manyTillIncludesEnd` legacyKarma))

    -- Identify each substring's karma type
    let karmaTag = map identifyKarma msg
    let karma = mapMaybe (\a -> case (snd a) of
                                    (Just k) -> Just $ Karma k (T.strip $ T.toLower $ T.pack $ fst a)
                                    Nothing  -> Nothing)
                    karmaTag

    if null karma
    then return Nothing
    else return $ Just karma

manyTillIncludesEnd :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a] -> ParsecT s u m [a]
manyTillIncludesEnd p end = scan
    where
        scan = (end >>= return) <|> do { x <- p; xs <- scan; return (x:xs) }

legacyKarma :: ParsecT T.Text u Identity String
legacyKarma = do
    a <- many1 (choice $ (try . string) `fmap`
        [ "++"
        , "--"
        , "+-"
        , "-+"
        , "±"
        , "∓"
        , "—" -- For those poor osx users
        , "＋＋"
        , "ーー"
        , "＋ー"
        , "ー＋"
        , "⊕⊕"
        , "⊖⊖"
        , "⊕⊖"
        , "⊖⊕"
        , "⊞⊞"
        , "⊟⊟"
        , "⊞⊟"
        , "⊟⊞"
        , "ⴱⴱ"
        , "ⴲⴲ"
        , "ⴲⴱ"
        , "ⴱⴲ"
        ])
    return $ concat a







-- Protype of the new karma parser

-- Brace choice Breakdown:
--  - From a corpus of 25,000 irc messages
--
-- {} - 77   - 28941
-- `` - 206  - 80813
-- [] - 231  - 215417
-- <> - 544  - 161483
-- "" - 982  - 549037
-- () - 1570 - 1013058
-- '' - 1691 - 1602736

-- TODO: decide what brace we want to use
leftBrace :: ParsecT T.Text u Identity Char
leftBrace = char '('

-- TODO: decide what brace we want to use
rightBrace :: ParsecT T.Text u Identity Char
rightBrace = char ')'

-- Karma
-- Check for one total karma
--  - If there is one, eat the rest and return the whole thing
--  - If there isn't one, eat the rest and check that there is at least 2 and return the whole thing
karma :: Config -> ParsecT T.Text u Identity String
karma conf = do
    firstTotal <- optionMaybe $ oneOf $ karmaTotal conf

    case firstTotal of
        Just x -> restOfKarma conf >>= \y -> return $ x:y
        Nothing -> concat `fmap` sequenceA [count 2 (oneOf $ karmaCandidate conf), restOfKarma conf]
    where
        karmaTotal = map fst . totalKarma
        karmaCandidate conf = map fst (partialKarma conf) ++ map fst (totalKarma conf)
        restOfKarma = many . oneOf . karmaCandidate


nonKarmaParse :: ParsecT T.Text u Identity KarmaCandidates
nonKarmaParse = KarmaNonCandidate <$> many1 anyChar

simpleKarmaParse :: Config -> ParsecT T.Text u Identity KarmaCandidates
simpleKarmaParse conf = KarmaCandidate
    <$> anyChar `manyTill` (lookAhead ((karma conf) >> notFollowedBy rightBrace))
    <*> (karma conf)

bracedKarmaParse :: Config -> ParsecT T.Text u Identity KarmaCandidates
bracedKarmaParse conf = KarmaCandidate
    <$> between (many1 leftBrace) (many1 rightBrace) (anyChar `manyTill` (lookAhead ((many1 rightBrace) >> (karma conf))))
    <*> (karma conf)

-- TODO: fix up this one to properly do karma subparser
nonBracedKarmaParse :: Config -> ParsecT T.Text u Identity KarmaCandidates
nonBracedKarmaParse conf = KarmaNonCandidate <$> many1 (noneOf "+(")

nonKarmaPreBracedKarmaParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
nonKarmaPreBracedKarmaParse conf = sequenceA [nonBracedKarmaParse conf, bracedKarmaParse conf]

eatKarmaInsideBracesParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
eatKarmaInsideBracesParse conf = do
    before <- many1 leftBrace
    expr <- anyChar `manyTill` (lookAhead ((many1 rightBrace) >> notFollowedBy (karma conf)))
    after <- many1 rightBrace

    return [KarmaNonCandidate (before ++ expr ++ after)]

nestedKarmaParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
nestedKarmaParse conf = concat `fmap` many1 (choice
        [ try (nonKarmaPreBracedKarmaParse conf)
        , fmap pure $ try (bracedKarmaParse conf)
        , fmap pure $ try (simpleKarmaParse conf)
        , try (eatKarmaInsideBracesParse conf)
        , fmap pure $ nonKarmaParse
        ])







-- TODO: Finally take the KarmaCandidate list and process then to determite which ones are
--  ACTUAL Karma results to be emitted to stdout

identifyKarma :: String -> (String, Maybe KarmaType)
identifyKarma msg =
    if T.null $ T.strip $ T.pack msg
    then (msg, Nothing)
    else
        let (m1, k1) = (init msg, last msg) in
        if T.null $ T.strip $ T.pack m1
        then (m1, Nothing)
        else case (singleKarmaType k1) of
            (Just k) -> (m1, Just k)
            Nothing  -> do
                let (m2, k2) = L.splitAt (L.length msg - 2) msg
                if T.null $ T.strip $ T.pack m2
                then (m2, Nothing)
                else (m2, karmaType k2)

-- TODO: unify the karmaType, singleKarmaType and karma parser to use a unified karma Type database
--  Externalize this to a configuration file, can probably do - list of - up/down/side, and upvote/sidevote/downvote and then process them.
karmaType :: String -> Maybe KarmaType
karmaType "++"   = Just Upvote
karmaType "--"   = Just Downvote
karmaType "+-"   = Just Sidevote
karmaType "-+"   = Just Sidevote
karmaType "＋＋" = Just Upvote
karmaType "ーー" = Just Downvote
karmaType "＋ー" = Just Sidevote
karmaType "ー＋" = Just Sidevote
karmaType "⊕⊕"   = Just Upvote
karmaType "⊖⊖"   = Just Upvote
karmaType "⊕⊖"   = Just Sidevote
karmaType "⊖⊕"   = Just Sidevote
karmaType "⊞⊞"   = Just Upvote
karmaType "⊟⊟"   = Just Upvote
karmaType "⊞⊟"   = Just Sidevote
karmaType "⊟⊞"   = Just Sidevote
karmaType "ⴱⴱ"   = Just Upvote
karmaType "ⴲⴲ"   = Just Upvote
karmaType "ⴲⴱ"   = Just Sidevote
karmaType "ⴱⴲ"   = Just Sidevote
karmaType _      = Nothing

singleKarmaType :: Char -> Maybe KarmaType
singleKarmaType '±' = Just Sidevote
singleKarmaType '∓' = Just Sidevote
singleKarmaType '—' = Just Downvote
singleKarmaType _   = Nothing
