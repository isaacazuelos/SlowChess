module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified CoordTests as Coord (tests)
import qualified FIDE  (tests)

main = defaultMain $ testGroup "SlowChess tests" [ FIDE.tests, Coord.tests]
