{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MoveInternalTests (tests) where

import           Instances                    ()

import           Data.Monoid                  ((<>))

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.List                    ((\\))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Piece

-- | Collects the targets of some 'Move's into a mask, just a helper.
targets = foldl (\ a m -> a <> maybe 0 mask (destination m)) 0

tests = testGroup "Move.Internal Tests"
    [ testSteps, testCasts, testEach ]

testSteps = testGroup "Test stepping"
    [testStepOff, testStepBlank, testNoStepFriendly, testStepEnemy]

testStepOff = testCase "Stepping off the board"
    (step White starting S (coord A1) @?= [])

testStepBlank = testCase "Steps onto blanks"
    (step White starting N (coord A2) @?= [coord A3])

testNoStepFriendly = testCase "Won't step onto friendly"
    (step White starting S (coord A2) @?= [])

testStepEnemy = testCase "Will step onto enemies"
    (step White b N (coord A1) @?= [coord A2])
  where b = setMany blank [(White, King, [A1]), (Black, King, [A2])]

-- cast :: [Direction] -> Piece -> Colour -> Board -> [Ply]
testCasts = testGroup "Test casting"
    [testCastOff, testCastBlanks, testCastFriends, testCastEnemies]

castBoard :: Board
castBoard = setMany blank [(Black, Queen, [A1])]

testCastOff = testCase "Casting off board"
    (cast [S] Queen Black castBoard @?= [])

testCastBlanks = testCase "Casting onto blanks"
    (targets result @?= fromList [B1 .. H1])
  where result = cast [E] Queen Black castBoard

testCastFriends = testCase "Casting stops before friendlies"
    (targets result @?= fromList [B1 .. D1])
  where result = cast [E] Queen Black b
        b = setMany castBoard [(Black, Rook, [E1])]

testCastEnemies = testCase "Casting stops at enemies"
    (targets result @?= fromList [B1 .. E1])
  where result = cast [E] Queen Black b
        b = setMany castBoard [(White, Rook, [E1])]

testEach = testCase "Test each on a staring board"
    (each White Pawn starting \\ map coord [A2 .. H2] @?= [])
