{-# LANGUAGE OverloadedStrings #-}
import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.KarmatorRoute


--
-- Actually run the tests
--
main :: IO ()
main = defaultMain $ concat
    [ hUnitTestToTests routingTests
    ]
