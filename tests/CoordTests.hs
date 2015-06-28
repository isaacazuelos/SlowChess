module CoordTests (tests) where

import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck (testProperty)

import           Test.HUnit                          (Assertion, (@=?))
import           Test.QuickCheck

import           Game.SlowChess.Coord
import           Game.SlowChess.Piece

tests :: Test
tests = testGroup "Coord Tests"
            [ testProperty "Test Coord--Coordname Enums" testEnums
            , testProperty "Test Coord hopping"          testHop
            , testCase     "Test hopping off board"      testHopOff
            ]

-- the Enum instances of Coord and CoordName should line up properly.
testEnums :: Int -> Property
testEnums e =   (e > 0 && e <= 64)
            ==> (fromEnum . name . coord . toEnum) e
            ==  (fromEnum . coord . name . toEnum) e

-- hopping north is the same as incrementing the coord up by eight.
testHop :: Int -> Property
testHop e = (e > 0 && e <= 55) ==> hop N (toEnum e) == toEnum (e + 8)

testHopOff :: Assertion
testHopOff = hop N (coord A8) @=? coord OffBoard
