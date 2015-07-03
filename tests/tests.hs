{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Test.Tasty (defaultMain, testGroup)

import qualified BoardTests as Board (tests)
import qualified CoordTests as Coord (tests)
import qualified FIDETests  as FIDE (tests)
import qualified MaskTests  as Mask (tests)
import qualified PieceTests as Piece (tests)

main = defaultMain $ testGroup "SlowChess tests" [ Piece.tests
                                                 , Mask.tests
                                                 , Coord.tests
                                                 , Board.tests
                                                 , FIDE.tests
                                                 ]
