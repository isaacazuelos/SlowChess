module Main where

import Test.Framework (defaultMain, Test)

import qualified CoordTests as Coord (tests)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ Coord.tests ]
