module MovementTests (tests) where

import           Game.SlowChess.Movement

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

import qualified Data.Set                as S

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Movement Tests" [ stepBlanksTests
                                   , stepCaptureTest
                                   , castBlanksTest
                                   , castCapTest
                                   ]

stepBlanksTests :: TestTree
stepBlanksTests = testGroup "Step Test" (map (uncurry testDir) [ (SW, -9)
                                                               , (S,  -8)
                                                               , (SE, -7)
                                                               , (W,  -1)
                                                               , (E,   1)
                                                               , (NW,  7)
                                                               , (N,   8)
                                                               , (NE,  9)
                                                               ])
  where testDir d n = testCase ("step " ++ show d)
                      (step Black Queen blanks testBoard d
                       @?= [ blank { queens = fromList [27 + n]
                                   , blacks = fromList [27 + n]}])


stepCaptureTest :: TestTree
stepCaptureTest = testCase "step capture"
                  (step Black Queen (material White) testCapBoard S
                   @?= [ blank { queens = fromList [19]
                               , blacks = fromList [19]}])

castBlanksTest :: TestTree
castBlanksTest = testCase "cast onto blanks"
                 (S.fromList (cast Black Queen testBoard S)
                  @?= S.fromList [ blank { queens = fromList [19]
                                         , blacks = fromList [19]}
                                 , blank { queens = fromList [11]
                                         , blacks = fromList [11]}
                                 , blank { queens = fromList [3]
                                         , blacks = fromList [3]}
                                 ])

castCapTest :: TestTree
castCapTest = testCase "cast onto blanks, capture, stop"
                 (S.fromList (cast Black Queen testCastCapBoard S)
                  @?= S.fromList [ blank { queens = fromList [19]
                                         , blacks = fromList [19]
                                         , pawns  = fromList [11]
                                         , whites = fromList [11]}
                                 , blank { queens = fromList [11]
                                         , blacks = fromList [11]}
                                 ])

-- Test Boards

-- 8|_|_|_|_|_|_|_|_|
-- 7|_|_|_|_|_|_|_|_|
-- 6|_|_|_|_|_|_|_|_|
-- 5|_|_|_|_|_|_|_|_|
-- 4|_|_|_|Q|_|_|_|_|
-- 3|_|_|_|_|_|_|_|_|
-- 2|_|_|_|_|_|_|_|_|
-- 1|_|_|_|_|_|_|_|_|
--   a b c d e f g h
testBoard :: Board
testBoard = blank {queens = fromList [27], blacks = fromList [27]}

-- testBoard with a white pawn south of the black queen.
testCapBoard :: Board
testCapBoard = set White Pawn testBoard (fromList [19])

testCastCapBoard :: Board
testCastCapBoard = set White Pawn testBoard (fromList [11])
