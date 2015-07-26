-- |
-- Module      : Game.SlowChess.Move.Promotion
-- Description : Movement definition for both basic and special movements
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- When a pawn gets to the far side of the board, it /must/ be replaced with a
-- queen, rook, bishop, or knight. This is checked after each move, so
-- challange don't necessarily do this.

module Game.SlowChess.Move.Promotion ( PromoteTo ( ToQueen
                                                 , ToRook
                                                 , ToBishop
                                                 , ToKnight
                                                 )
                                     , mustPromote
                                     , promotions
                                     ) where

import           Data.Monoid                  ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask          hiding (fromList)
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

-- | Does a player have pawns that need to be promoted?
mustPromote :: Game -> Bool
mustPromote g = case ply g of
    Just (Move c Pawn _ t) -> mask t `submask` furthestRank c
    _ -> False

promote :: Game -> PromoteTo -> [Game]
promote g pt = case ply g of
    Just (Move c' Pawn s t) -> if t `on` furthestRank c'
                                  then newGame s t
                                  else []
    _ -> []
  where newBoard t  = update (player g) (toPiece pt) (board g) (<> mask t)
        newPly      = Promotion (enemy (player g)) (toPiece pt)
        newGame s t = return $ g { ply = Just $ newPly s t
                                 , board = newBoard t
                                 }

-- | The ranks which are the furthest from the starting rank for a colour.
-- Pawns on the their furthest rank /must/ be promoted.
furthestRank :: Colour -> Mask
furthestRank White = fromList [A8, B8, C8, D8, E8, F8, G8, H8]
furthestRank Black = fromList [A1, B1, C1, D1, E1, F1, G1, H1]

-- | Perform all promotions that apply to a game.
promotions :: Game -> [Game]
promotions g = if null gs then return g else gs
  where gs = [ToQueen, ToRook, ToBishop, ToKnight] >>= promote g
