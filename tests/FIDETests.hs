{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FIDETests (tests) where

import           Test.Tasty                   (testGroup)
import           Test.Tasty.HUnit             (assert, testCase, (@?), (@?=))

import           Data.Maybe                   (mapMaybe)

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask          hiding (fromList)
import           Game.SlowChess.Move
import           Game.SlowChess.Move.Castle
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Piece

-- These tests are the examples given in the FIDE Laws of Chess, specifically
-- the ones for after July 1st, 2014. Although, to be honest, I'm not familiar
-- enought with Chess politics to know what the differences are.
-- <https://www.fide.com/component/handbook/?id=171&view=article>

tests = testGroup "FIDE tests" [ article1, article3 ]

article1 = testGroup "Article 1" [ testCheckmate ]

testCheckmate = testGroup "1.2 - Checkmate Rules"
    [ testInCheckmate, testNotInCheckmate ]

checkmateGame1 :: Game
checkmateGame1 = challange Black $ setMany blank [ (White, King, [F5])
                                                 , (Black, King, [H5])
                                                 , (White, Rook, [H1])
                                                 ]

checkmateGame2 :: Game
checkmateGame2 = challange Black $ setMany blank [ (Black, King, [H5]) ]

testInCheckmate = testCase "Checkmate in affermative" $
    checkmate checkmateGame1 @?
        show (board checkmateGame1) ++ "\nAppearently is not in checkmate."

testNotInCheckmate = testCase "Checkmate in negative" $
    not (checkmate checkmateGame2) @?
        show (board checkmateGame2) ++ "\nAppearently is in checkmate."

article3 = testGroup "Article 3" [ testBishopMove
                                 , testRookMove
                                 , testQueenMove
                                 , testCastBlocking
                                 , testKnightMoves
                                 , testPawnMoves
                                 , testKingMoves
                                 , testCheck
                                 ]

testBishopMove = testCase "3.2 - Bishops move diagonally" (result @?= expected)
  where result   = targets $ moveBishops Black b
        b        = setMany blank [(Black, Bishop, [E4])]
        expected = fromList [ B1, C2, D3, F5, G6, H7, A8
                            , B7, C6, D5, F3, G2, H1
                            ]

testRookMove = testCase "3.3 - Rooks move along their current rank or file"
                (result @?= expected)
  where result   = targets $ moveRooks Black b
        b        = setMany blank [(Black, Rook, [D3])]
        expected = fromList [ A3, B3, C3, E3, F3, G3, H3
                            , D1, D2, D4, D5, D6, D7, D8
                            ]

testQueenMove = testCase "3.4 - Queens move like rooks and bishops"
    (result @?= expected)
  where expected = fromList [ B1, C2, D3, F5, G6, H7     -- SW to NE
                            , A4, B4, C4, D4, F4, G4, H4 -- E  to W
                            , E1, E2, E3, E5, E6, E7, E8 -- S  to N
                            , A8, B7, C6, D5, F3, G2, H1 -- NE to SW
                            ]
        result = targets $ moveQueens Black
                    (setMany blank [(Black, Queen, [E4])])

-- Note that this test expects rooks to move correctly.
testCastBlocking = testCase ("3.5 - Bishops, rooks and queens can't move" ++
    " through pieces.") (assert . not $ invalid `submask` result)
  where invalid = fromList [A3 .. A8]
        result  = targets $ moveRooks Black b
        b   = setMany blank [ (Black, Rook, [A1])
                                , (White, Pawn, [A2, B1])
                                ]

testKnightMoves = testGroup "Secion 3.6" [ testKnightMoveBlank, testKnightHops ]

knightTestBoard = setMany blank [ (White, Knight, [C3])
                                , (Black, Knight, [G8])
                                , (Black, Pawn,   [F7, G7, H7])
                                , (Black, Rook,   [H8])
                                , (Black, Bishop, [F7])
                                ]

testKnightMoveBlank = testCase "Knight movement onto blank squares"
    (result @?= expected)
  where expected = fromList [B5, D5, E4, E2, D1, B1, A2, A4]
        result   = targets $ moveKnights White knightTestBoard

testKnightHops = testCase "Knight hopping over pieces" (result @?= expected)
  where expected = fromList [E7, F6, H6]
        result   = targets $ moveKnights Black knightTestBoard

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

testPawnStep = testCase "3.7.a - Panws moving one square"
                (result @?= fromList [G4])
  where result = targets $ movePawns Black pawnTestBoard1

-- Note that this is likely to fail if 'testPawnStep' does too.
testPawnStepTwice = testCase "3.7.b - Pawns at starting position move twice"
                        (result @?= fromList [C3, C4])
  where result = targets (movePawns White pawnTestBoard1)

testPawnAttack = testCase "3.7.c - Pawns attack diagonally forward"
                    (result @?= fromList [F4, G4] )
  where result = targets (movePawns Black pawnTestBoard2)

testEnPassant = testCase "3.7.d - En passant moves"
                 (expected `elem` results @?
                    "no EnPassant in: " ++ show results)
  where expected = EnPassant White (coord D5) (coord E6) (coord E5)
        results  = mapMaybe ply (future game >>= future)
        game = challange Black $ setMany blank [ (Black, Pawn, [E7])
                                               , (White, Pawn, [D5])
                                               ]

testPromotion = testCase "3.7.e - Pawn promotion"
    (expected `elem` result @? "no promotion in:\n\t" ++ show result)
  where expected = Promotion White Queen (coord A7) (coord A8)
        newBoard = challange White (set White Pawn blank (fromList [A7]))
        result = mapMaybe ply $ future newBoard

testKingMoves = testGroup "Section 3.8 - Kings" [testKingSteps, testCastling]

testKingSteps = testCase "3.8.a - Kings step 1 square" (result @?= expected)
  where result   = targets $ moveKings White b
        b        = setMany blank [(White, King, [C3, E8])]
        expected = fromList [ B2, C2, D3, B3, D3, B4, C4
                            , D4, D8, F8, D7, E7, F7, D2
                            ]

testCastling = testGroup "3.8.b - Castling" [ testBlackKingside
                                            , testWhiteQueenside
                                            , testCastleAttack
                                            , testCastleJump
                                            ]

castleGame :: Colour -> Game
castleGame c = enableAllCastleOptions (challange c b)
  where b = setMany blank [ (Black, King, [E8]), (Black, Rook, [H8])
                          , (White, Rook, [A1]), (White, King, [E1])
                          ]

testBlackKingside = testCase "Black kingside castling" (result @?= expected)
  where result   = mapMaybe ply $ castle (castleGame Black)
        expected = [Castle Black Kingside]

testWhiteQueenside = testCase "White queenside castling" (result @?= expected)
  where result   = mapMaybe ply $ castle (castleGame White)
        expected = [Castle White Queenside]

castleGame2 :: Game
castleGame2 = enableAllCastleOptions (challange White b)
  where b = setMany blank [ (White, Rook,  [A1]), (White, King, [E1])
                          , (Black, Queen, [B2])
                          ]

testCastleAttack = testCase "3.8.b.2.a"
    (Castle White Queenside `notElem` plys @? "No castle in:\n" ++ show plys)
  where plys = mapMaybe ply (future castleGame2)

castleGame3 :: Game
castleGame3 = enableAllCastleOptions $ challange White b
  where b = setMany blank [ (White, Rook,   [A1]), (White, King, [E1])
                          , (White, Knight, [B1])
                          ]

testCastleJump = testCase "3.8.b.2.b"
    (castle castleGame3 @?= [])

testCheck = testGroup "3.9 - Check rules" [ testInCheck, testNotInCheck ]

checkGame1 :: Game
checkGame1 = challange White $ setMany blank [ (White, King,  [A1])
                                             , (Black, Queen, [A8])
                                             , (Black, King,  [D4])
                                             ]

checkGame2 :: Game
checkGame2 = challange White $ setMany blank [ (White, King, [A1])
                                             , (Black, King, [H8])
                                             ]

testInCheck = testCase "3.9 - Check in the affermative" $ check checkGame1
    @? show (board checkGame1) ++ "\nAppearently isn't in check."

testNotInCheck = testCase "3.9 - Check in the negative" $
    not (check checkGame2) @?
        show (board checkGame2) ++ "\nAppearently is in check."
