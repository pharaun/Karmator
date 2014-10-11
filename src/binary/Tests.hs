{-# LANGUAGE OverloadedStrings #-}
import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.KarmaParser
import Tests.KarmatorRoute


--
-- Actually run the tests
--
main :: IO ()
main = defaultMain $ concat
    [ hUnitTestToTests nickNameTests
    , hUnitTestToTests botFilteringTests
    , hUnitTestToTests commandParsingTests
    , hUnitTestToTests newKarmaEdgeCaseTests
    , hUnitTestToTests karmaEdgeCaseTests
    , hUnitTestToTests routingTests
    ]
