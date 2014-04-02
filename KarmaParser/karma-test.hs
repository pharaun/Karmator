{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Test.QuickCheck

import qualified Data.Text as T
import qualified Data.List as L

import Data.Maybe (catMaybes)

import Text.Parsec
import Parser.Karma
import Parser.Types

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
    putStrLn "Testing new karma edge cases"
    let newKarmaTest = buildNewKarmaTests newKarmaData
    print =<< runTestTT newKarmaTest

    putStrLn ""
    putStrLn "Karma Edge case tests:"
    let karmaTest = buildKarmaTests karmaData
    print =<< runTestTT karmaTest

--
-- Nick name Defuzzifier tests
--
buildNickTests = TestList . map (\(src, dst) -> TestLabel (src ++ " -> " ++ dst) (nickTest src dst))
nickTest src dst = TestCase (assertEqual "" (Just $ T.pack dst) (either (const Nothing) Just (parse nickDeFuzzifier "(stdin)" $ T.pack src)))

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
-- NewKarmaType test cases
--
makeKarmaConfig = Config [] [] [('+', Up), ('-', Down)] [('±', Sidevote), ('∓', Sidevote)] '(' ')'
buildNewKarmaTests = TestList . map (\(str, karma) -> TestLabel "" (newKarmaTest str karma))
newKarmaTest str karma = TestCase (assertEqual "" (Just karma) (either (const Nothing) Just (parse (nestedKarmaParse makeKarmaConfig) "(stdin)" $ T.pack str)))

newKarmaData :: [(String, [KarmaCandidates])]
newKarmaData =
    [ ("a", [KarmaNonCandidate "a"])
    , ("a++", [KarmaCandidate "a" "++"])
    , ("a++++", [KarmaCandidate "a" "++++"])
    , ("a b++", [KarmaCandidate "a b" "++"])
    , ("a+ b++", [KarmaCandidate "a+ b" "++"])
    , ("a+b++", [KarmaCandidate "a+b" "++"])
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

    -- Weird karma
    , ("++", [KarmaCandidate "" "++"])
    , ("+++", [KarmaCandidate "" "+++"])
    , ("++++", [KarmaCandidate "" "++++"])
    , (" ++", [KarmaCandidate " " "++"])

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
    , ("a--b++", [KarmaCandidate "a" "--", KarmaCandidate "b" "++"])
    ]

--
-- Karma Parsing tests
--
makeKarmaParseConfig = Config [] [] [('+', Up), ('-', Down)] [('±', Sidevote), ('∓', Sidevote)] '(' ')'
buildKarmaTests = TestList . map (\(src, dst) -> TestLabel "" (karmaTest src dst))
karmaTest str result = TestCase (assertEqual "" result (either (const []) id (parse (karmaParse makeKarmaParseConfig) "(stdin)" $ T.pack str)))


karmaData :: [(String, [Karma])]
karmaData =
    [ ("", [])
    , ("a", [])
    , ("a+", [])
    , ("a-", [])

    -- TODO: do we want to disallow this case
    , ("++", []) -- ACK - #4
    , ("+++", [Karma Upvote "+"])
    , ("++++", [Karma Upvote "++"])
    , (" ++", []) -- ACK - #7

    -- Parse trivial karma
    , ("a++", [Karma Upvote "a"])
    , ("a--", [Karma Downvote "a"])
    , ("a+-", [Karma Sidevote "a"])
    , ("a-+", [Karma Sidevote "a"])
    , ("a±", [Karma Sidevote "a"])

    -- Parse excess karma
    , ("a+++", [Karma Upvote "a+"])
    , ("a-++", [Karma Upvote "a-"])
    , ("a++++", [Karma Upvote "a++"])
    , ("a--++", [Karma Upvote "a--"])
    , ("++--", [Karma Downvote "++"])
    , ("--++", [Karma Upvote "--"])

    , ("a±±", [Karma Sidevote "a±"])
    , ("a+±", [Karma Sidevote "a+"])

    -- TODO: not sure about this one, should this results into an karma or should we fail?
    , ("a+±+", [Karma Sidevote "a+"])

    -- Whitespace karma
    , ("a b++", [Karma Upvote "a b"])
    , ("a+ b++", [Karma Upvote "a+ b"]) -- ACK - #23
    , ("a++b++", [Karma Upvote "a++b"]) -- ACK - #24
    , ("a++b", [])

    -- Braced karma
    , ("(a)++", [Karma Upvote "a"])
    , ("(a++)++", [Karma Upvote "a++"])
    , ("(a++", [Karma Upvote "(a"])
    , ("a)++", [Karma Upvote "a)"])
    , ("(a) b++", [Karma Upvote "(a) b"])

    -- Nested braces
    , ("((a))++", [Karma Upvote "(a)"])
    , ("((a++))++", [Karma Upvote "(a++)"])
    , ("((a++)++", [Karma Upvote "(a++"])
    , ("(a++))++", [Karma Upvote "a++)"])
    , ("((a)) b++", [Karma Upvote "((a)) b"])
    , ("(a (b))++", [Karma Upvote "a (b)"])
    , ("(++a)++", [Karma Upvote "++a"])
    , ("a(b)c++", [Karma Upvote "a(b)c"])
    , ("((a)++)++", [Karma Upvote "(a)++"]) -- ACK - #39

    -- White space in braces
    , ("( a)++", [Karma Upvote "a"])
    , ("(a )++", [Karma Upvote "a"])
    , ("( a )++", [Karma Upvote "a"])
    , ("((a) )++", [Karma Upvote "(a)"]) -- ACK - #43
    , ("( (a))++", [Karma Upvote "(a)"])
    , ("( (a) )++", [Karma Upvote "(a)"]) -- ACK - #45
    , ("((a)++ )++", [Karma Upvote "(a)++"]) -- ACK - #46
    , ("((a++)++ )++", [Karma Upvote "(a++)++"]) -- ACK - #47

    -- Braced non-candidates
    , ("(a)", [])
    , ("(a++)", [])
    , ("((a))", [])
    , ("((a++))", [])
    , ("((a)++)", [])
    , ("a (b++)", [])
    , ("(a b++)", [])
    , ("(a (b++))", [])

    -- Unicode
    , ("私を++", [Karma Upvote "私を"])
    , ("♥++", [Karma Upvote "♥"])
    , ("とある.NET++", [Karma Upvote "とある.net"])

    -- Unusual
    , ("/\\.?code/i--", [Karma Downvote "/\\.?code/i"])

    -- Basic multi-karma
    , ("a++ b++", [Karma Upvote "a", Karma Upvote "b"])
    , ("ikea++ modular furniture++ meatballs++", [Karma Upvote "ikea", Karma Upvote "modular furniture", Karma Upvote "meatballs"])

    -- Gnu args and preinc karma
    , ("++anonymous whoever", [])
    , ("improvement and ++karma", [])
    , ("twistd web --root=docs/build/html but whatev", [])
    , ("++a++", [Karma Upvote "++a"])

    -- Inline karma should not karma
    , ("how about xn--asdfasdf.co.jp", [])
    , ("nickname, yeah--i was going to do this", [])
    , ("Fri: -15f--4F | sat: -31F--34F", [])
    , (">8-D-|--<", [])

    -- Deal with gnu params
    , ("web --root", [])
    , ("web --root --or --bar", [])

    -- Odd ones
    , ("(web --root=docs --or --foobar)++", [Karma Upvote "web --root=docs --or --foobar"])
    , ("web --root=docs --or --foobar++", [Karma Upvote "web --root=docs --or --foobar"]) -- ACK - #72
    ]


--    , ("a: ++", [Karma Upvote "a"])
--
--    -- Multi-karma in one expression
--    , ("btw, a++ and b++ for this", [Karma Upvote "a", Karma Upvote "b"])
--    , ("Solved. a++, b++", [Karma Upvote "a", Karma Upvote "b"])
--
--
--    -- "Clearly" Invalid karma which should result in nothing
--    , (":,::--_._,:---.", [])
--    , (" c      - +i-- ", [])
--    , ("--i22:----.vSe.", [])
--    , ("XZQ[==|++===+==+`", [])
--    , ("#Z#(|i>|+=+|++=|](<>):(j[[", [])
--    , ("suppose -+ and +- should", [])
--    , ("basename: invalid option -- 'h'", [])
--    , ("\"hah something --- without whatever\"", [])
--    , ("dont ++ me", [])
--    , ("-------------", [])
--    , ("+----+----+", [])
--    , (":--(", [])
--    , ("++ stuff", [])
--    , ("something: ( <-- womp ) something else", [])
--    , ("Warning, sphere corruption at twenty-- rats cannot throw up.", [])
--    , ("-rw-r--r-- 1 user users 215k 2012-08-14 12:13 something/path", [])

-- that definitely doesn't merit a --
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