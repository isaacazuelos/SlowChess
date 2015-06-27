-- |
-- Module      : Game.SlowChess.Move.Promotion
-- Description : Movement definition for both basic and special movements
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- When a pawn gets to the far side of the board, it /must/ be replaced with a
-- queen, rook, bishop, or knight.

module Game.SlowChess.Move.Promotion where

import           Data.Maybe                   (isJust, listToMaybe)
import           Data.Monoid                  ((<>))

import           Game.SlowChess.Mask hiding (fromList)
import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game.Internal
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
promoted :: PromoteTo -> Piece
promoted ToQueen  = Queen
promoted ToRook   = Rook
promoted ToBishop = Bishop
promoted ToKnight = Knight

-- | Does the most recent ply in the game's history require that a piece be
-- promoted? Note that at most one piece can require promotion, so there's
-- no need specify which.
mustPromote :: Game -> Bool
mustPromote g = isJust $ do (_, ply) <- listToMaybe (history g)
                            target   <- targetOf ply
                            if target `on` furthestRank (player g)
                            then Nothing
                            else Just ()

-- | The ranks which are the furthest from the starting rank for a colour.
-- Pawns on the their 'furthestRank' /must/ be promoted.
furthestRank :: Colour -> Mask
furthestRank White = fromList [A8, B8, C8, D8, E8, F8, G8, H8]
furthestRank Black = fromList [A1, B1, C1, D1, E1, F1, G1, H1]

-- | Promote the last piece to move in a game to the select piece. This
-- function /does not/ check that the promotion be legal --- to do that use
-- 'mustPromote'. If there isn't a last piece to promote (becasue there isn't
-- a history, the last move was a castle, etc.) then 'Nothing' is returned.
promote :: Game -> PromoteTo -> Maybe Game
promote g pt = do (b, ply) <- listToMaybe (history g)
                  target    <- targetOf ply
                  let p       = promoted pt
                  let newBoard = update (player g) p (board g) (<> mask target)
                  newPly   <- toPromotion ply
                  return g { board = newBoard
                           , history = (b, newPly) : tail (history g)
                           }
  where toPromotion (Move c p s t) = Just (Promotion c p s t)
        toPromotion _ = Nothing
