{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Test.QuickCheck

import qualified Data.Text as T

import Text.Parsec
import Parser.Karma
import Parser.Types

karma n = either (\_ -> Nothing) (\a -> Just a) (parse (karmaParse $ Config [] [] [] [] '(' ')') "(stdin)" $ T.pack n)
nick n = either (\_ -> Nothing) (\a -> Just a) (parse nickDeFuzzifier "(stdin)" $ T.pack n)

main :: IO ()
main = do
    putStrLn "Nickname tests:"
    let nickTest = buildNickTests nickData
    print =<< runTestTT nickTest

    putStrLn ""
    putStrLn "Bot Filtering tests:"
    let filterTest = buildFilterTests botData
    print =<< runTestTT filterTest

    putStrLn ""
    putStrLn "Quickcheck Karma Type:"

    putStrLn ""
    putStrLn "Karma Edge case tests:"
    let karmaTest = buildKarmaTests karmaData
    print =<< runTestTT karmaTest

    putStrLn ""
    putStrLn "Testing new karma edge cases"
    let newKarmaTest = buildNewKarmaTests newKarmaData
    print =<< runTestTT newKarmaTest

--
-- Nick name Defuzzifier tests
--
buildNickTests = TestList . map (\(src, dst) -> TestLabel (src ++ " -> " ++ dst) (nickTest src dst))
nickTest src dst = TestCase (assertEqual ("Nickname DeFuzzifier: " ++ src ++ " -> " ++ dst) (Just $ T.pack dst) (nick src))

nickData :: [(String, String)]
nickData =
    [ ("Nick", "Nick")
    , ("Nick1", "Nick")
    , ("Nick_", "Nick")
    , ("__Nick", "Nick")
    , ("Nick_wfh", "Nick")
    , ("Nick|BR", "Nick")
    , ("Nick^pub", "Nick")
    , ("Nick_1", "Nick")
    , ("Nick-home", "Nick")
    , ("Nick[FT-mobile]", "Nick")
    , ("Nick`", "Nick")
    , ("Nick^", "Nick")

    -- Acknowledged failures
    , ("Nick0", "Nick") -- Should be: Nick0
    , ("Nicktron", "Nicktron") -- Should be: Nick
    , ("ghost_of_Nick", "ghost") -- Should be: Nick
    , ("iNick4S", "iNick") -- Should be: Nick
    , ("pharaun", "pharaun") -- Should be: Nick
    , ("push-Nick", "push") -- Should be: Nick
    , ("interview^Nick", "interview") -- Should be: Nick
    ]

--
-- Identifying bots and filtering
--
makeConfig = Config ["karmator"] ["websphere"] [] [] '(' ')'
buildFilterTests = TestList . map (\bot  -> TestLabel bot (botFilterTest bot))
botFilterTest bot = TestCase (assertEqual ("Filters out: " ++ bot) True (filterBot makeConfig $ T.pack bot))

botData :: [String]
botData =
    [ "websphere"
    , "websphere10"
    , "websphere20-websphere"
    , "websphereLA"
    , "karmator"
    ]

--
-- KarmaType quickcheck
--


--
-- NewKarmaType test cases
--
makeKarmaConfig = Config [] [] [('+', Up), ('-', Down)] [('±', Sidevote), ('∓', Sidevote)] '(' ')'
buildNewKarmaTests = TestList . map (\(str, karma) -> TestLabel "" (newKarmaTest str karma))
newKarmaTest str karma = TestCase (assertEqual "" (Just karma) (either (\_ -> Nothing) (\a -> Just a) (parse (nestedKarmaParse $ makeKarmaConfig) "(stdin)" $ T.pack str)))

newKarmaData :: [(String, [KarmaCandidates])]
newKarmaData =
    [ ("a", [KarmaNonCandidate "a"])
    , ("a++", [KarmaCandidate "a" "++"])
    , ("a++++", [KarmaCandidate "a" "++++"])
    , ("a b++", [KarmaCandidate "a b" "++"])
    , ("a++ b", [KarmaCandidate "a" "++", KarmaNonCandidate " b"])
    , ("a++ b++", [KarmaCandidate "a" "++", KarmaCandidate " b" "++"])
    , ("(a)++", [KarmaCandidate "a" "++"])
    , ("(a++)++", [KarmaCandidate "a++" "++"])
    , ("(a)++ b", [KarmaCandidate "a" "++", KarmaNonCandidate " b"])
    , ("(a)++ b++", [KarmaCandidate "a" "++", KarmaCandidate " b" "++"])
    , ("(a++)++ b", [KarmaCandidate "a++" "++", KarmaNonCandidate " b"])
    , ("(a++)++ b++", [KarmaCandidate "a++" "++", KarmaCandidate " b" "++"])
    , ("a++ (b)", [KarmaCandidate "a" "++", KarmaNonCandidate " (b)"])
    , ("(a++", [KarmaCandidate "(a" "++"])
    , ("a)++", [KarmaCandidate "a)" "++"])
    , ("((a))", [KarmaNonCandidate "((a))"])
    , ("(a) b++", [KarmaCandidate "(a) b" "++"])
    , ("((a)) b++", [KarmaCandidate "((a)) b" "++"])
    , ("a (b)++", [KarmaNonCandidate "a ", KarmaCandidate "b" "++"])
    , ("a (b++)++", [KarmaNonCandidate "a ", KarmaCandidate "b++" "++"])

    -- TESTS: uncertain about behavors -- Probably better to only parse one set of braces
    , ("((a))++", [KarmaCandidate "(a)" "++"])
    , ("a (b++)++", [KarmaNonCandidate "a ", KarmaCandidate "b++" "++"])
    , ("a ((b++))++", [KarmaNonCandidate "a ", KarmaCandidate "(b++)" "++"])
    , ("(a (b))++", [KarmaCandidate "a (b)" "++"])
    , ("((a (b)))++", [KarmaCandidate "(a (b))" "++"])
    , ("((a) b))++", [KarmaCandidate "(a) b)" "++"])

    -- TESTS: Should extend the parser to care about karma inside braces only if there's no karma outside braces?
    -- Maybe a nice way to "escape" karma without karma'ing
    , ("(a++)", [KarmaNonCandidate "(a++)"])
    , ("((a++))", [KarmaNonCandidate "((a++))"])
    , ("a (b++)", [KarmaNonCandidate "a (b++)"])
    , ("a ((b++))", [KarmaNonCandidate "a ((b++))"])
    , ("(a++) b", [KarmaNonCandidate "(a++)", KarmaNonCandidate " b"])
    , ("((a++)) b", [KarmaNonCandidate "((a++))", KarmaNonCandidate " b"])

    -- Additional wrinkles re we want to eat some of this but not others
    , ("(a++) b++", [KarmaNonCandidate "(a++)", KarmaCandidate " b" "++"])
    , ("((a++)) b++", [KarmaNonCandidate "((a++))", KarmaCandidate " b" "++"])

    -- Partial karma (we want to be rightmost)
    , ("a+++", [KarmaCandidate "a" "+++"])
    , ("a+±+", [KarmaCandidate "a" "+±+"])
    , ("a+±", [KarmaCandidate "a" "+±"])
    , ("a±", [KarmaCandidate "a" "±"])
    , ("a+", [KarmaNonCandidate "a+"])

    -- Concat/preinc karma
    , ("++a", [KarmaCandidate "" "++", KarmaNonCandidate "a"])
    , ("++a++", [KarmaCandidate "" "++", KarmaCandidate "a" "++"])
    , ("(++a)++", [KarmaCandidate "++a" "++"])

    -- Brace whitespaces
    , ("( a)++", [KarmaCandidate " a" "++"])
    , ("(a )++", [KarmaCandidate "a " "++"])
    , ("( a )++", [KarmaCandidate " a " "++"])
    , ("( (a) )++", [KarmaCandidate " (a) " "++"])
    , ("((a) )++", [KarmaCandidate "(a) " "++"])
    , ("( (a))++", [KarmaCandidate " (a)" "++"])

    -- Complicated brace/karma nesting
    , ("((a)++)++", [KarmaCandidate "(a)++" "++"])
    , ("((a)++ )++", [KarmaCandidate "(a)++ " "++"])
    , ("((a++)++ )++", [KarmaCandidate "(a++)++ " "++"])

    -- Misc
    , ("./bin --gnu-lol", [KarmaCandidate "./bin " "--", KarmaNonCandidate "gnu-lol"])
    , ("a--b", [KarmaCandidate "a" "--", KarmaNonCandidate "b"])
    ]


--
-- Karma Parsing tests
--
buildKarmaTests = TestList . map (\(src, dst) -> TestLabel (src ++ " -> " ++ (show dst)) (karmaTest src dst))
karmaTest src dst = TestCase (assertEqual ("Karma Edgecases: " ++ src ++ " -> " ++ (show dst)) (prepareData dst) (karma src))
prepareData xs
    | null xs   = Just (Nothing)
    | otherwise = Just (Just xs)

karmaData :: [(String, [Karma])]
karmaData =
    -- Trivial cases first
--    [ ("", [])
    [ ("a++", [Karma Upvote "a"])
    , ("a--", [Karma Downvote "a"])
    , ("a+-", [Karma Sidevote "a"])
    , ("a-+", [Karma Sidevote "a"])

    -- Multi-karma cases
    , ("a++", [Karma Upvote "a"])
    , ("a+++", [Karma Upvote "a"])
    , ("a++++", [Karma Upvote "a++"])
    , ("a+++++", [Karma Upvote "a++"])
    , ("++--", [Karma Downvote "++"])
    , ("--++", [Karma Upvote "--"])

    -- Multi-karma in one expression
    , ("a++ a++", [Karma Upvote "a", Karma Upvote "a"])
    , ("a++ b++", [Karma Upvote "a", Karma Upvote "b"])
    , ("ikea++ modular furniture++ meatballs++", [Karma Upvote "ikea", Karma Upvote "modular furniture", Karma Upvote "meatballs"])
    , ("a: ++", [Karma Upvote "a"])
    , ("btw, a++ and b++ for this", [Karma Upvote "a", Karma Upvote "b"])
    , ("Solved. a++, b++", [Karma Upvote "a", Karma Upvote "b"])

    -- Karma within braces and quotations
    , ("(a b)++", [Karma Upvote "a b"])
    , ("{a b}++", [Karma Upvote "a b"])
    , ("[a b]++", [Karma Upvote "a b"])
    , ("<a b>++", [Karma Upvote "a b"])
    , ("'a b'++", [Karma Upvote "a b"])
    , ("`a b`++", [Karma Upvote "a b"])
    , ("\"a b\"++", [Karma Upvote "a b"])
    , ("(a--)++", [Karma Upvote "a--"])
    , ("mac'n'cheese++", [Karma Upvote "mac'n'cheese"])

    , ("(a b++", [Karma Upvote "(a b"])
    , ("{a b++", [Karma Upvote "{a b"])
    , ("[a b++", [Karma Upvote "[a b"])
    , ("<a b++", [Karma Upvote "<a b"])
    , ("'a b++", [Karma Upvote "'a b"])
    , ("`a b++", [Karma Upvote "`a b"])
    , ("\"a b++", [Karma Upvote "\"a b"])

    , ("a b)++", [Karma Upvote "a b)"])
    , ("a b}++", [Karma Upvote "a b}"])
    , ("a b]++", [Karma Upvote "a b]"])
    , ("a b>++", [Karma Upvote "a b>"])
    , ("a b'++", [Karma Upvote "a b'"])
    , ("a b`++", [Karma Upvote "a b`"])
    , ("a b\"++", [Karma Upvote "a b\""])

    -- Unicode
    , ( "私を++", [Karma Upvote "私を"])
    , ( "♥++", [Karma Upvote "♥"])
    , ("とある.NET++", [Karma Upvote "とある.NET"])

    -- Unusual
    , ( "++", [])
    , ("/\\.?code/i—", [Karma Downvote "/\\.?code/i—"])

    -- "Clearly" Invalid karma which should result in nothing
    , (":,::--_._,:---.", [])
    , (" c      - +i-- ", [])
    , ("--i22:----.vSe.", [])
    , ("XZQ[==|++===+==+`", [])
    , ("#Z#(|i>|+=+|++=|](<>):(j[[", [])
    , ("nickname, yeah--i was going to do this", [])
    , ("improvement and ++karma", [])
    , ("twistd web --root=docs/build/html but whatev", [])
    , ("how about xn--asdfasdf.co.jp", [])
    , ("suppose -+ and +- should", [])
    , ("basename: invalid option -- 'h'", [])
    , ("\"hah something --- without whatever\"", [])
    , ("++anonymous whoever", [])
    , ("dont ++ me", [])
    , ("-------------", [])
    , ("+----+----+", [])
    , (":--(", [])
    , ("++ stuff", [])
    , ("something: ( <-- womp ) something else", [])
    , ("https://maps.google.com/maps?q=26.383258,+-79.978409", [])
    , ("Warning, sphere corruption at twenty-- rats cannot throw up.", [])
    , ("-rw-r--r-- 1 user users 215k 2012-08-14 12:13 something/path", [])
    , ("Fri: -15f--4F | sat: -31F--34F", [])
    , ("http://www.something/Salad-Fingers-Episode-5--Salad-Fingers--Picnic.jpg", [])
-- that definitely doesn't merit a --
-- >8-D-|--<
-- that sounds like it should prevent ++a++ too

    -- Nickname upvotes in various form
    --
    -- Awesome! Nick++
    -- Nick++ testing stuff
    -- Nick1++ and Nick2++
    -- Nick++!!
    -- Ok then. Nick++
    -- ＳＮＯＯＰ ＬＩＯＮ++

    -- Nickname with topic
    --
    -- Nick1, Nick2: good bread++
    -- Nick: sounds good. Push++
    -- Nick: karma++

    -- Braces
    --
    -- ""ergonomic" keyboards"++
    -- (socialist stroopwafel) policies+-
    -- socialist (stroopwafel policies)++
    -- commun(ity)ism++
    -- dev<prime_number>++
    -- ObjectionalLanguage: "Nine years of ballet, asshole."++
    -- (not using --no-notification)--

    -- Warning: Sorry, can not save your changes. This ticket has been modified by someone else since you started--
    ]
