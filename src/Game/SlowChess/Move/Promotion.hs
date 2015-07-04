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
                                     , promotions
                                     , promote
                                     ) where

import           Game.SlowChess.Mask hiding (fromList)
import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Move.Internal
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
mustPromote c b = 0 /= get c Pawn b `both` furthestRank c

-- | The ranks which are the furthest from the starting rank for a colour.
-- Pawns on the their furthest rank /must/ be promoted.
furthestRank :: Colour -> Mask
furthestRank White = fromList [A8, B8, C8, D8, E8, F8, G8, H8]
furthestRank Black = fromList [A1, B1, C1, D1, E1, F1, G1, H1]

-- | Promotes a the ply of a pawn moving to the back rank to a promotion of
-- the specified 'PromoteTo' piece.
promote :: Ply -> PromoteTo -> Ply
promote m@(Move c Pawn s t) pt = if t `on` furthestRank c
                                 then Promotion c (toPiece pt) s t
                                 else m
promote p _ = p

-- | Run through a list of plys and promote the ones that can be promoted.
promotions :: [Ply] -> [Ply]
promotions ps = do pt <- [ToQueen, ToRook, ToKnight, ToBishop]
                   m  <- ps
                   return $ promote m pt
