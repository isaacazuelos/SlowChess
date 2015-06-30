-- |
-- Module      : Game.SlowChess.Move.Promotion
-- Description : Movement definition for both basic and special movements
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- When a pawn gets to the far side of the board, it /must/ be replaced with a
-- queen, rook, bishop, or knight.

module Game.SlowChess.Move.Promotion ( PromoteTo ( ToQueen
                                                 , ToRook
                                                 , ToBishop
                                                 , ToKnight
                                                 )
                                     , mustPromote
                                     , promote
                                     ) where

import           Data.Monoid ((<>))

import           Game.SlowChess.Mask hiding (fromList)
import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Piece

-- | Restricts a promotion to the legal pieces -- namely queen, rook, bishop,
-- or knight.
data PromoteTo = ToQueen
               | ToRook
               | ToBishop
               | ToKnight
               deriving ( Show, Eq )

-- | Get the piece type to promote to.
toPiece :: PromoteTo -> Piece
toPiece ToQueen  = Queen
toPiece ToRook   = Rook
toPiece ToBishop = Bishop
toPiece ToKnight = Knight

-- | Does a player have pawns the need to promote?
mustPromote :: Colour -> Board -> Bool
mustPromote c b = 0 /= backPawns c b

-- | The ranks which are the furthest from the starting rank for a colour.
-- Pawns on the their 'furthestRank' /must/ be promoted.
furthestRank :: Colour -> Mask
furthestRank White = fromList [A8, B8, C8, D8, E8, F8, G8, H8]
furthestRank Black = fromList [A1, B1, C1, D1, E1, F1, G1, H1]

-- | All the pawns of a colour on a board on that colour's furthest rank.
backPawns :: Colour -> Board -> Mask
backPawns c b = get c Pawn b `both` furthestRank c

-- | Promotes all the pawns of a colour on the furthest rank to the
-- specified 'PromoteTo' piece.
promote :: Colour -> Board -> PromoteTo -> Board
-- Since there should only ever be one pawn on the back rank (since they
-- /must/ be promoted), this isn't that bad.
promote c b pt = set c p b promoted
  where p = toPiece pt
        promoted = backPawns c b <> get c p b
