module Main where

import           Nestest.Spec as Nestest
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
    Nestest.test
  -- , Blargg.test
  ]

