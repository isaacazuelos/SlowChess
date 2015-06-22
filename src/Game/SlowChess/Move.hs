-- |
-- Module      : Game.SlowChess.Move
-- Description : Movement definition for both basic and special movements.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Game.SlowChess.Move where

import Game.SlowChess.Board
import Game.SlowChess.Mask
import Game.SlowChess.Piece

-- | Represents the changes made by in a single ply.
--
--   * When a 'Move' happens, the source becomes blank and the target is
--     overwritten with the piece moved.
data Ply = Move { source :: Mask
                , target :: Mask
                } deriving (Show, Eq)

-- | Does a ply move to any of the places indicated in the mask?
-- Typically this is used with the mask used to filter non-friendly
-- targets for plys.
lands :: Mask -> Ply -> Bool
lands m p = 0 /= both m (target p)

-- | Generates all the valid movements of the king of a colour on a
-- board. Kings can move in any direction so long as they stay on the board
-- and don't step on any friendly material.
moveKings :: Colour -> Board -> [Ply]
moveKings c b = (filter validTargets) (allDirections >>= step King c b)
  where validTargets = lands (invert $ material c b)

-- | Step each piece of a kind and colour on a board in a direction, with no
-- regard for if the step is onto a valid location.
step :: Piece -> Colour -> Board -> Direction -> [Ply]
step p c b d = map (\ m -> Move m (hop d m)) (each p c b)

-- | Returns a list of each of the specified kind of piece, and the
-- initial board with that piece removed.
each :: Piece -> Colour -> Board -> [Mask]
each p c b = split (get c p b)
