-- |
-- Module      : Game.SlowChess.Move.Internal
-- Description : Movement definition for both basic and special movements
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Game.SlowChess.Move.Internal where

import           Control.Monad        (mzero)

import           Data.Monoid          ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

-- | Represents the changes made by in a single ply.
--
--   * When a 'Move' happens, the source becomes blank and the target is
--     overwritten with the piece moved.
data Ply = Move Colour Piece Mask Mask deriving (Show, Eq)

-- | Move out in straight lines along the directions until either the pieces
-- choose to stop, further motion would mean stepping on a friendly unit, or œthe
-- last step taken removed an enemy unit.
moveByCasting :: [Direction] -> Piece -> Colour -> Board -> [Ply]
moveByCasting ds p c b = do direction <- ds
                            source    <- each c p b
                            target    <- cast c b direction source
                            return $ Move c p source target

-- | Does a ply's target move it to any of the places indicated in the mask?
-- Typically this is used with the mask as a filter for to pick out blanks or
-- enemies.
--
-- For example, to compute where knights might land on blanks you might do:
--
-- > do p <- each Black Knight staring)
-- >    t <- jumpKnight p
-- >    lands (blanks b) t
lands :: Mask -> Mask -> [Mask]
lands m p = if p `on` m then return p else mzero

-- | Are the tiles of the first mask a subset of the second mask's tiles?
-- Returns False if the first mask if 0, since that already means the piece is
-- off the board.
on :: Mask -> Mask -> Bool
on s m = s /= 0 && (s <> m) == m

-- | Step each piece of a kind and colour on a board in a direction, with no
-- regard for if the step is onto a valid location, returns the board.
stepAny :: Direction -> Mask -> [Mask]
stepAny d m = if hop d m == 0 then mzero else return (hop d m)

-- | Step each piece of a kind and colour on a board in a direction, landing
-- only on non-friendly squares  --- i.e. blanks or enemy units.
step :: Colour -> Board -> Direction -> Mask -> [Mask]
step c b d s = stepAny d s >>= lands (nonFriendly c b)

-- | Steps a piece over blank squares until it hits (and captures) a single
-- enemy. This is the type of motion used by rooks, bishops and queens.
cast :: Colour -> Board -> Direction -> Mask -> [Mask]
cast c b d s = do candidate <- step c b d s
                  if candidate `on` blanks b
                    then candidate : cast c b d candidate
                    else if candidate `on` material (enemy c) b
                         then return candidate
                         else mzero

-- | Returns a list of each of/ the specified kind of piece on a mask of it's
-- own.
each :: Colour -> Piece -> Board -> [Mask]
each c p b = split (get c p b)

-- | Change a board by applying the move from a ply.
apply :: Board -> Ply -> Board
apply b (Move c p s t) = update c p b (\ m -> t <> (m `minus` s))
