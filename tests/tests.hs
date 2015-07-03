{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified BoardTests as Board (tests)
import qualified CoordTests as Coord (tests)
import qualified FIDE  (tests)

main = defaultMain $ testGroup "SlowChess tests" [ Coord.tests
                                                 , Board.tests
                                                 , FIDE.tests
                                                 ]
