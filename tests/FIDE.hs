module FIDE (tests) where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assert, (@?=))

import           Data.Monoid                    ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask            hiding (fromList)
import           Game.SlowChess.Move
import           Game.SlowChess.Move.Castle
import           Game.SlowChess.Move.EnPassant
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Move.Promotion
import           Game.SlowChess.Piece

-- These tests are the examples given in the FIDE Laws of Chess, specifically
-- the ones for after July 1st, 2014. Although, to be honest, I'm not familiar
-- enought with Chess politics to know what the differences are.
-- <https://www.fide.com/component/handbook/?id=171&view=article>

tests :: Test
tests = testGroup "FIDE tests" [ article3 ]

article3 :: Test
article3 = testGroup "Article 3" [ testBishopMove
                                 , testRookMove
                                 , testQueenMove
                                 , testCastBlocking
                                 , testKnightMoves
                                 , testPawnMoves
                                 , testKingMoves
                                 ]

-- | Collects the targets of some 'Move's into a mask, just a helper.
targets :: [Ply] -> Mask
targets = foldl (\ a m -> a <> maybe 0 mask (targetOf m)) 0

testBishopMove :: Test
testBishopMove = testCase "3.2 - Bishops move diagonally" (result @?= expected)
  where result   = targets $ moveBishops Black b
        b        = setMany blank [(Black, Bishop, [E4])]
        expected = fromList [ B1, C2, D3, F5, G6, H7, A8
                            , B7, C6, D5, F3, G2, H1
                            ]


testRookMove :: Test
testRookMove = testCase "3.3 - Rooks move along their current rank or file"
                (result @?= expected)
  where result   = targets $ moveRooks Black b
        b    = setMany blank [(Black, Rook, [D3])]
        expected = fromList [ A3, B3, C3, E3, F3, G3, H3
                            , D1, D2, D4, D5, D6, D7, D8
                            ]

testQueenMove :: Test
testQueenMove = testCase "3.4 - Queens move like rooks and bishops"
    (result @?= expected)
  where expected = fromList [ B1, C2, D3, F5, G6, H7     -- SW to NE
                            , A4, B4, C4, D4, F4, G4, H4 -- E  to W
                            , E1, E2, E3, E5, E6, E7, E8 -- S  to N
                            , A8, B7, C6, D5, F3, G2, H1 -- NE to SW
                            ]
        result = targets $ moveQueens Black
                    (setMany blank [(Black, Queen, [E4])])

testCastBlocking :: Test
-- Note that this test expects rooks to move correctly.
testCastBlocking = testCase ("3.5 - Bishops, rooks and queens can't move" ++
    " through pieces.") (assert . not $ invalid `submask` result)
  where invalid = fromList [A3 .. A8]
        result  = targets $ moveRooks Black b
        b   = setMany blank [ (Black, Rook, [A1])
                                , (White, Pawn, [A2, B1])
                                ]

testKnightMoves :: Test
testKnightMoves = testGroup "Secion 3.6" [ testKnightMoveBlank, testKnightHops ]

knightTestBoard :: Board
knightTestBoard = setMany blank [ (White, Knight, [C3])
                                , (Black, Knight, [G8])
                                , (Black, Pawn,   [F7, G7, H7])
                                , (Black, Rook,   [H8])
                                , (Black, Bishop, [F7])
                                ]

testKnightMoveBlank :: Test
testKnightMoveBlank = testCase "Knight movement onto blank squares"
    (result @?= expected)
  where expected = fromList [B5, D5, E4, E2, D1, B1, A2, A4]
        result   = targets $ moveKnights White knightTestBoard

testKnightHops :: Test
testKnightHops = testCase "Knight hopping over pieces" (result @?= expected)
  where expected = fromList [E7, F6, H6]
        result   = targets $ moveKnights Black knightTestBoard

testPawnMoves :: Test
testPawnMoves = testGroup "Section 3.7" [ testPawnStep
                                        , testPawnStepTwice
                                        , testPawnAttack
                                        , testEnPassant
                                        , testPromotion
                                        ]

-- | This test board doesn't have any non-pawn pieces on it, like the on in
-- the rule book.
pawnTestBoard1 :: Board
pawnTestBoard1 = setMany blank [(White, Pawn, [C2]), (Black, Pawn, [G5])]

pawnTestBoard2 :: Board
pawnTestBoard2 = setMany pawnTestBoard1 [ (White, Queen, [F4])
                                        , (Black, Queen, [H4])
                                        ]

testPawnStep :: Test
testPawnStep = testCase "3.7.a - Panws moving one square"
                (result @?= fromList [G4])
  where result = targets $ movePawns Black pawnTestBoard1

testPawnStepTwice :: Test
-- Note that this is likely to fail if 'testPawnStep' does too.
testPawnStepTwice = testCase "3.7.b - Pawns at starting posiiton move twice"
                        (result @?= fromList [C3, C4])
  where result = targets (movePawns White pawnTestBoard1)

testPawnAttack :: Test
testPawnAttack = testCase "3.7.c - Pawns attack diagonally forward"
                    (result @?= fromList [F4, G4] )
  where result = targets (movePawns Black pawnTestBoard2)

testEnPassant :: Test
testEnPassant = testCase "3.7.d - En passant moves"
                 (result @?= fromList [E6, D6])
  where oldBoard = setMany blank [ (Black, Pawn, [E7])
                                 , (White, Pawn, [D5])
                                 ]
        lastMove = StepTwice Black (coord E7) (coord E5) (coord E6)
        result   = targets . enPassant $ game
        game     = Game { player = Black
                        , board = setMany blank [ (Black, Pawn, [E5])
                                                , (White, Pawn, [D5])]
                        , history = [(oldBoard, lastMove)]
                        }

testPromotion :: Test
testPromotion = testGroup "3.7.e - Pawn promotion"
                    [ testPromotionDetection, testPromote ]

promotionBoard :: Board
promotionBoard = set White Pawn blank (fromList [A8])

promotedBoard :: Board
promotedBoard = set White Queen blank (fromList [A8])

testPromotionDetection :: Test
testPromotionDetection = testCase "Pawn promition detection"
    (assert $ mustPromote White promotionBoard)

testPromote :: Test
testPromote = testCase "Pawn promotion" (result @?= promotedBoard)
  where result = promote White promotionBoard ToQueen

testKingMoves :: Test
testKingMoves = testGroup "Section 3.8 - Kings" [testKingSteps, testCastling]

testKingSteps :: Test
testKingSteps = testCase "3.8.a - Kings step 1 square" (result @?= expected)
  where result   = targets $ moveKings White b
        b        = setMany blank [(White, King, [C3, E8])]
        expected = fromList [ B2, C2, D3, B3, D3, B4, C4
                            , D4, D8, F8, D7, E7, F7, D2
                            ]

testCastling :: Test
testCastling = testGroup "3.8.b - Castling" [ testBlackKingside
                                            , testWhiteQueenside
                                            , testCastleKingMoved
                                            , testCastleRookMoved
                                            ]

castleGame :: Colour -> Game
castleGame c = Game { player = c
                    , history = []
                    , board = setMany blank [ (Black, King, [E8])
                                            , (Black, Rook, [H8])
                                            , (White, Rook, [A1])
                                            , (White, King, [E1])
                                            ]
                    }

testBlackKingside :: Test
testBlackKingside = testCase "Black kingside castling" (result @?= expected)
  where result   = castle (castleGame Black)
        expected = [Castle Black Kingside]

testWhiteQueenside :: Test
testWhiteQueenside = testCase "White queenside castling" (result @?= expected)
  where result   = castle (castleGame White)
        expected = [Castle White Queenside]

testCastleKingMoved :: Test
testCastleKingMoved = testCase "You can't castle if the king has moved"
    (castle game @?= [])
  where move = Move White King (coord E2) (coord E1)
        game = (castleGame White) { history = rewrittenHistory }
        rewrittenHistory = [(oldBoard,move)]
        oldBoard = setMany blank [ (Black, King, [E8]), (Black, Rook, [H8])
                                 , (White, Rook, [A1]), (White, King, [E2])
                                 ]

testCastleRookMoved :: Test
testCastleRookMoved = testCase "You can't caslte if the rook has moved"
                        (castle game @?= [])
  where move = Move White Rook (coord A2) (coord A1)
        game = (castleGame White) { history = rewrittenHistory }
        rewrittenHistory = [(oldBoard,move)]
        oldBoard = setMany blank [ (Black, King, [E8]), (Black, Rook, [H8])
                                 , (White, Rook, [A2]), (White, King, [E1])
                                 ]

-- TODO: Implement tests for 3.8.b.2
-- TODO: Implement tests for 3.9
