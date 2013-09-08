{-# LANGUAGE OverloadedStrings #-}
module Parser.Karma
    ( nickDeFuzzifier
    , filterBot
    , karmaParse
    , nestedKarmaParse
    ) where

import qualified Data.Text as T

-- Parsec
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Functor.Identity (Identity)

import qualified Data.List as L
import Data.Maybe

import Control.Applicative hiding ((<|>), many, optional)

import Data.Traversable (sequenceA)

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
filterBot c nick = nick `elem` strictMatchList c || L.any (`T.isPrefixOf` nick) (prefixMatchList c)



-- TODO:
--  Clean up and standardize the resulting karma string
--  - Text.ICU.Normalize
--  - Normalize it to NFKC
--  - Casefold(?) or would just lowercase be acceptable
karmaParse :: Config -> ParsecT T.Text u Identity (Maybe [Karma])
karmaParse conf = processCandidates conf `fmap` nestedKarmaParse conf
    where
        processCandidates :: Config -> [KarmaCandidates] -> Maybe [Karma]
        processCandidates conf candidates
            -- If no candidate just exit
            | L.null candidates = Nothing

            -- If only one possible candidate just process it and exit early
            | L.length candidates == 1 = case head candidates of
                KarmaNonCandidate{} -> Nothing
                KarmaCandidate{kcMessage=msg, kcKarma=karma} ->
                    case evalulateKarma conf karma of
                        (e, Just k)  -> Just [Karma k (T.toLower $ T.pack $ msg ++ e)] -- TODO: add T.strip maybe
                        (_, Nothing) -> Nothing

            -- TODO: Process the rest here
            | otherwise = do
                Nothing


-- Evalulate in a rightmost manner, and return the remainder of the karma
evalulateKarma :: Config -> String -> (String, Maybe KarmaType)
evalulateKarma conf karma
    | L.null karma        = ("", Nothing)
    | L.length karma == 1 = lookupTotalKarma conf karma
    | otherwise           = lookupPartialKarma conf karma

    where
        lookupTotalKarma conf karma =
            let tk = L.lookup (L.head $ L.reverse karma) $ totalKarma conf
            in case tk of
                Just k  -> (L.take (L.length karma - 1) karma, Just k)
                Nothing -> ("", Nothing)

        lookupPartialKarma conf karma =
            let pk  = L.lookup (L.head $ L.reverse karma) $ partialKarma conf
                pk' = L.lookup (L.head $ L.tail $ L.reverse karma) $ partialKarma conf
            in case (pk, pk') of
                (Nothing, _) -> lookupTotalKarma conf karma
                (Just _, Nothing) -> lookupTotalKarma conf (L.init karma)
                (Just k, Just k') -> (L.take (L.length karma - 2) karma, Just $ partalToTotal k k')

        partalToTotal Up Up = Upvote
        partalToTotal Down Down = Downvote
        partalToTotal _ _ = Sidevote


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
leftBrace :: Config -> ParsecT T.Text u Identity Char
leftBrace = char . openBrace

rightBrace :: Config -> ParsecT T.Text u Identity Char
rightBrace = char . closeBrace


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
    <$> anyChar `manyTill` lookAhead (karma conf >> notFollowedBy (rightBrace conf))
    <*> karma conf

bracedKarmaParse :: Config -> ParsecT T.Text u Identity KarmaCandidates
bracedKarmaParse conf = do
    before <- L.drop 1 `fmap` many1 (leftBrace conf)
    expr <- anyChar `manyTill` lookAhead (many1 (rightBrace conf) >> karma conf)

    a <- many1 $ rightBrace conf
    let after = L.take (L.length a - 1) a

    karma <- karma conf

    return $ KarmaCandidate (before ++ expr ++ after) karma

-- TODO: fix up this one to properly do karma subparser
nonBracedKarmaParse :: Config -> ParsecT T.Text u Identity KarmaCandidates
nonBracedKarmaParse conf = KarmaNonCandidate <$> many1 (noneOf (openBrace conf : karmaCandidate conf))
    where
        karmaCandidate conf = map fst (partialKarma conf) ++ map fst (totalKarma conf)

nonKarmaPreBracedKarmaParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
nonKarmaPreBracedKarmaParse conf = sequenceA [nonBracedKarmaParse conf, bracedKarmaParse conf]

eatKarmaInsideBracesParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
eatKarmaInsideBracesParse conf = do
    before <- many1 $ leftBrace conf
    expr <- anyChar `manyTill` lookAhead (many1 (rightBrace conf) >> notFollowedBy (karma conf))
    after <- many1 $ rightBrace conf

    return [KarmaNonCandidate (before ++ expr ++ after)]

nestedKarmaParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
nestedKarmaParse conf = concat `fmap` many1 (choice
        [ try (nonKarmaPreBracedKarmaParse conf)
        , fmap pure $ try (bracedKarmaParse conf)
        , fmap pure $ try (simpleKarmaParse conf)
        , try (eatKarmaInsideBracesParse conf)
        , fmap pure nonKarmaParse
        ])
