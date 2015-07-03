{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Test.Tasty        (defaultMain, testGroup)

import qualified BoardTests        as Board (tests)
import qualified CoordTests        as Coord (tests)
import qualified FIDETests         as FIDE (tests)
import qualified GameInternalTests as GameInternal (tests)
import qualified MaskTests         as Mask (tests)
import qualified MoveInternalTests as MoveInternal (tests)
import qualified PieceTests        as Piece (tests)

-- the order of the tests should roughly reflect their interdependencies.

main = defaultMain $ testGroup "SlowChess tests" [ Piece.tests
                                                 , Mask.tests
                                                 , Coord.tests
                                                 , Board.tests
                                                 , GameInternal.tests
                                                 , MoveInternal.tests
                                                 , FIDE.tests
                                                 ]

-- moveByCasting :: [Direction] -> Piece -> Colour -> Board -> [Ply]
-- targetOf :: Ply -> Maybe Coord
-- piece :: Ply -> Maybe Piece
-- lands :: Mask -> Coord -> [Coord]
-- stepAny :: Direction -> Coord -> [Coord]
-- step :: Colour -> Board -> Direction -> Coord -> [Coord]
-- cast :: Colour -> Board -> Direction -> Coord -> [Coord]
-- each :: Colour -> Piece -> Board -> [Coord]

testTargetOf = testGroup "Test targetOf" []
