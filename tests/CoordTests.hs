{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CoordTests (tests) where

import           Instances             ()

import           Data.Monoid           (mempty)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Game.SlowChess.Coord
import qualified Game.SlowChess.Mask   as M
import           Game.SlowChess.Piece

tests = testGroup "Coord Tests" [ testCoordEnum
                                , testFromList
                                , testMergeSplit
                                , testOn
                                , testNotOn
                                , testNowhereIsNotOn
                                , testHopOff
                                ]

-- the Enum instances of Coord and CoordName should line up properly.
testCoordEnum = testProperty "Test coord-name alignment"
    (\ n -> n >= 0 && n < 64 ==>
        (fromEnum . coord . toEnum) n == (fromEnum . name . toEnum) n)

testFromList = testProperty "Coord.fromList matches Mask.fromList"
    (\ xs -> OffBoard `notElem` xs ==> fromList xs == (M.fromList . map fromEnum) xs)

testMergeSplit = testProperty "merge undoes split"
    (\ m -> m == (foldr merge mempty . split) m)

testOn = testProperty "We can tell if a coord is on a mask"
    (\ c -> c `on` fromList [name c])

testNotOn = testProperty "We can tell if a coord is not on a mask"
    (\ c -> not (c `on` mempty))

testNowhereIsNotOn = testProperty "nowhere is never on"
    (\m -> not (nowhere `on` m))

testHopOff = testCase "Test hopping off boards"
    (hop N (coord A8) @=? coord OffBoard)
