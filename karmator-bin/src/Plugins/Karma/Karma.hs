{-# LANGUAGE OverloadedStrings #-}
module Plugins.Karma.Karma
    ( nickDeFuzzifier
    , filterBot
    , karmaCommandParse
    , karmaParse
    , nestedKarmaParse

    , chanParse
    ) where

import qualified Data.Text as T

-- Parsec
import Text.Parsec
import Data.Functor.Identity (Identity)

import qualified Data.List as L
import Data.Maybe

import Control.Applicative hiding ((<|>), many, optional)

import Data.Traversable (sequenceA)

import Plugins.Karma.Types


--
-- Karma Braces
--
-- TODO: Have a version of this that will eat whitspace and restore it to fix a few case
--
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
leftBrace :: ParsecT T.Text u Identity Char
leftBrace = char openBrace

rightBrace :: ParsecT T.Text u Identity Char
rightBrace = char closeBrace

openBrace = '['
closeBrace = ']'
quote = '"'


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
filterBot c nick = nick `elem` strictMatchList c || L.any (`T.isPrefixOf` nick) (prefixMatchList c) || L.any (`T.isSuffixOf` nick) (suffixMatchList c)


-- TODO: Break this out into its own module
chanParse :: ParsecT T.Text u Identity [T.Text]
chanParse = do
    cmdParse

    words <- many (noneOf [' ']) `sepEndBy` space
    eof

    return $ map T.pack $ filter (/= "") words

-- TODO: Break this out into its own module
-- TODO: fix this cases
--    , ("!karma ( (foo))", [" (foo)"])
--    , ("!karma ( (foo) )", [" (foo) "])
--    , ("!karma ( ( foo) )", [" ( foo) "])
--    , ("!karma ( ( foo ) )", [" ( foo ) "])
karmaCommandParse :: ParsecT T.Text u Identity [T.Text]
karmaCommandParse = do
    cmdParse

    words <- choice
        [ brace quote quote
        , brace openBrace closeBrace
        , simple
        ] `sepEndBy` space
    eof

    return $ map T.pack $ filter (/= "") words

cmdParse :: ParsecT T.Text u Identity ()
cmdParse = oneOf "!" >> skipMany1 letter >> optional spaces

simple :: ParsecT T.Text u Identity String
simple = many $ noneOf [' ', openBrace, closeBrace, quote]

--
-- Karma Command Braces
--
brace :: Char -> Char -> ParsecT T.Text u Identity String
brace open close = do
    before <- L.drop 1 `fmap` (many1 $ char open)
    expr <- many $ noneOf [close]

    a <- many1 $ char close
    let after = L.take (L.length a - 1) a

    return $ before ++ expr ++ after


-- TODO:
--  Clean up and standardize the resulting karma string
--  - Text.ICU.Normalize
--  - Normalize it to NFKC
--  - Casefold(?) or would just lowercase be acceptable
karmaParse :: Config -> ParsecT T.Text u Identity [Karma]
karmaParse conf = catMaybes `fmap` processCandidates conf `fmap` nestedKarmaParse conf
    where
        processCandidates :: Config -> [KarmaCandidates] -> [Maybe Karma]
        processCandidates _ [] = []
        processCandidates c [k@KarmaCandidate{}] = [evalKarma c k]
        processCandidates c (KarmaNonCandidate{}:ks) = processCandidates c ks

        -- TODO: implement this
        --  - For now just look ahead one, probably want to deal with looking ahead N times
        --  - Decide on if/how we want to deal with trimming?
        --  - Decide how to identify/deal with braced karma vs regular, may be worth uniquelly identifying these
        processCandidates c (k@KarmaCandidate{kcMessage=msg}:k'@KarmaCandidate{kcMessage=msg'}:ks)
        --  - (a++)++ -> (a++)++
            | safeLast msg /= " " && safeHead msg' == [closeBrace] = evalKarma c ( mergeKarma k k') : processCandidates c ks

        --  - a++ b++ -> a++,b++
        --  - a ++ b++ -> a ++,b++
            | safeHead msg' == " "  = evalKarma c k : processCandidates c (k':ks)

        --  - arg --gnulol foo++ -> ??
--            | safeLast msg == " " && safeHead msg' /= " " = processCandidates c (k':ks)

        --  - ++a++ -> (++a)++
            | msg == "" && safeHead msg' /= " " = evalKarma c (mergeKarma k k') : processCandidates c ks

        --  - a++b++ -> (a++b)++
            | safeLast msg /= " " && safeHead msg' /= " " && L.null ks = [evalKarma c $ mergeKarma k k']

        -- Everything else
            | otherwise = []

        processCandidates c (k@KarmaCandidate{kcMessage=msg}:k'@KarmaNonCandidate{kncMessage=msg'}:ks)
        --  - a++ b -> a++
        --  - a ++ b -> a ++
            | safeHead msg' == " "  = evalKarma c k : processCandidates c (k':ks)

        --  - arg --gnulol ->
            | safeLast msg == " " && safeHead msg' /= " " = processCandidates c (k':ks)

        --  - a++b ->
        --  - ++a ->
            | otherwise = []

        -- These are specced mostly for strings
        safeHead :: [a] -> [a]
        safeHead = L.take 1

        safeLast :: [a] -> [a]
        safeLast xs = L.drop (L.length xs - 1) xs

        evalKarma :: Config -> KarmaCandidates -> Maybe Karma
        evalKarma _ KarmaNonCandidate{} = Nothing
        evalKarma c KarmaCandidate{kcMessage=msg, kcKarma=karma} =
            case evalulateKarma c karma of
                (e, Just k)  ->
                    let msg' = T.strip (T.pack msg) `T.append` T.pack e
                    in if T.null msg' then Nothing else Just (Karma k msg')
                (_, Nothing) -> Nothing

        -- TODO: this is an incomplete function, it only deals with a few cases
        mergeKarma :: KarmaCandidates -> KarmaCandidates -> KarmaCandidates
        mergeKarma KarmaNonCandidate{kncMessage=msg} KarmaCandidate{kcMessage=msg', kcKarma=karma} = KarmaCandidate (msg ++ msg') karma
        mergeKarma KarmaCandidate{kcMessage=msg, kcKarma=karma} KarmaCandidate{kcMessage=msg', kcKarma=karma'} = KarmaCandidate (msg ++ karma ++ msg') karma'
        mergeKarma kc@KarmaCandidate{} KarmaNonCandidate{} = kc



-- Evalulate in a rightmost manner, and return the remainder of the karma
evalulateKarma :: Config -> String -> (String, Maybe KarmaType)
evalulateKarma conf karma = evalKarma conf $ L.reverse karma
    where
        evalKarma :: Config -> String -> (String, Maybe KarmaType)
        evalKarma _ []        = ("", Nothing)
        evalKarma c [k]       = lookupTotalKarma c [k]
        evalKarma c (k:k':ks) =
            let pk  = L.lookup k $ partialKarma c
                pk' = L.lookup k' $ partialKarma c
            in case (pk, pk') of
                (Nothing, _)       -> lookupTotalKarma c (k:k':ks)
                (Just _, Nothing)  -> lookupTotalKarma c (k':ks)
                (Just k1, Just k2) -> (L.reverse ks, Just $ partialToTotal k1 k2)

        lookupTotalKarma :: Config -> String -> (String, Maybe KarmaType)
        lookupTotalKarma _ []     = ("", Nothing)
        lookupTotalKarma c (k:ks) = case L.lookup k $ totalKarma c of
            Just k' -> (L.reverse ks, Just k')
            Nothing -> ("", Nothing)

        partialToTotal :: PartialKarmaType -> PartialKarmaType -> KarmaType
        partialToTotal Up Up = Upvote
        partialToTotal Down Down = Downvote
        partialToTotal _ _ = Sidevote


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
    <$> anyChar `manyTill` lookAhead (karma conf >> notFollowedBy rightBrace)
    <*> karma conf

bracedKarmaParse :: Config -> ParsecT T.Text u Identity KarmaCandidates
bracedKarmaParse conf = do
    before <- L.drop 1 `fmap` many1 leftBrace
    expr <- anyChar `manyTill` lookAhead (many1 rightBrace >> karma conf)

    a <- many1 rightBrace
    let after = L.take (L.length a - 1) a

    karma <- karma conf

    return $ KarmaCandidate (before ++ expr ++ after) karma

-- TODO: fix up this one to properly do karma subparser - Instead of just getting a list of karma character,
--      Should make sure it properly at least parse a valid karma otherwise fail
nonBracedKarmaParse :: Config -> ParsecT T.Text u Identity KarmaCandidates
nonBracedKarmaParse conf = KarmaNonCandidate <$> many1 (noneOf (openBrace : karmaCandidate conf))
    where
        karmaCandidate conf = map fst (partialKarma conf) ++ map fst (totalKarma conf)

nonKarmaPreBracedKarmaParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
nonKarmaPreBracedKarmaParse conf = sequenceA [nonBracedKarmaParse conf, bracedKarmaParse conf]

eatKarmaInsideBracesParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
eatKarmaInsideBracesParse conf = do
    before <- many1 leftBrace
    expr <- anyChar `manyTill` lookAhead (many1 rightBrace >> notFollowedBy (karma conf))
    after <- many1 rightBrace

    return [KarmaNonCandidate (before ++ expr ++ after)]

nestedKarmaParse :: Config -> ParsecT T.Text u Identity [KarmaCandidates]
nestedKarmaParse conf = concat `fmap` many1 (choice
        [ try (nonKarmaPreBracedKarmaParse conf)
        , pure <$> try (bracedKarmaParse conf)
        , pure <$> try (simpleKarmaParse conf)
        , try (eatKarmaInsideBracesParse conf)
        , fmap pure nonKarmaParse
        ])
