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
import           Game.SlowChess.Coord
import           Game.SlowChess.Mask  hiding (hop)
import           Game.SlowChess.Piece

-- | Represents the changes made by in a single ply.
--
--   * When a 'Move' happens, the source becomes blank and the target is
--     overwritten with the piece moved.
data Ply = Move      Colour Piece Coord Coord -- ^ player piece source target
         | EnPassant Colour Coord Coord Coord -- ^ player source target cap
         | StepTwice Colour Coord Coord Coord -- ^ player source target cap
         | Castle    Colour Side              -- ^ player side
         | Promotion Colour Piece Coord Coord -- ^ player piece source target
           deriving (Show, Eq)

-- | Colour-relative board sides, used for knowing which side a castle move
-- was done to.
data Side = Kingside | Queenside deriving ( Show, Eq )

-- | Move out in straight lines along the directions until either the pieces
-- choose to stop, further motion would mean stepping on a friendly unit, or
-- the last step taken removed an enemy unit.
moveByCasting :: [Direction] -> Piece -> Colour -> Board -> [Ply]
moveByCasting ds p c b = do direction <- ds
                            source    <- each c p b
                            target    <- cast c b direction source
                            return $ Move c p source target

-- Where the target square of a ply is, assuming that makes sense for the type
-- of movement.
targetOf :: Ply -> Maybe Coord
-- It's named `targtOf` rather than `target` since a log of exising code uses
-- `target` as an identifier. TODO: fix this
targetOf (Move _ _ s _) = Just s
targetOf _ = Nothing

-- | Return the type of piece that moved in a ply, if that makes sense.
-- Castling and doesn't have a clear piece.
piece :: Ply -> Maybe Piece
piece (Move      _ p _ _) = Just p
piece (Promotion _ p _ _) = Just p
piece (EnPassant _ _ _ _) = Just Pawn
piece _ = Nothing

-- | Does a ply's target move it to any of the places indicated in the mask?
-- Typically this is used with the mask as a filter for to pick out blanks or
-- enemies.
--
-- For example, to compute where knights might land on blanks you might do:
--
-- > do p <- each Black Knight staring)
-- >    t <- jumpKnight p
-- >    lands (blanks b) t
lands :: Mask -> Coord -> [Coord]
lands m p = if p `on` m then return p else mzero

-- | Step each piece of a kind and colour on a board in a direction, with no
-- regard for if the step is onto a valid location, returns the board.
stepAny :: Direction -> Coord -> [Coord]
stepAny d m = if hop d m == nowhere then mzero else return (hop d m)

-- | Step each piece of a kind and colour on a board in a direction, landing
-- only on non-friendly squares  --- i.e. blanks or enemy units.
step :: Colour -> Board -> Direction -> Coord -> [Coord]
step c b d s = stepAny d s >>= lands (nonFriendly c b)

-- | Steps a piece over blank squares until it hits (and captures) a single
-- enemy. This is the type of motion used by rooks, bishops and queens.
cast :: Colour -> Board -> Direction -> Coord -> [Coord]
cast c b d s = do candidate <- step c b d s
                  if candidate `on` blanks b
                    then candidate : cast c b d candidate
                    else if candidate `on` material (enemy c) b
                         then return candidate
                         else mzero

-- | Returns a list of each of/ the specified kind of piece on a mask of it's
-- own.
each :: Colour -> Piece -> Board -> [Coord]
each c p b = split (get c p b)

-- | Change a board by applying the move from a ply.
apply :: Board -> Ply -> Board
apply b (Move c p s t) = update c p b (\ m -> mask t <> m `minus` mask s)
apply b _ = b -- TODO: Update this for other move types.
