module Main where

import qualified MovementTests (tests)

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [MovementTests.tests]
