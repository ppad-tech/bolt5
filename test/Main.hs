module Main where

import qualified Lightning.Protocol.BOLT5 as BOLT5
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt5" [
    placeholder_tests
  ]

placeholder_tests :: TestTree
placeholder_tests = testGroup "Placeholder" [
    testCase "placeholder is unit" test_placeholder
  ]

test_placeholder :: Assertion
test_placeholder = BOLT5.placeholder @?= ()
