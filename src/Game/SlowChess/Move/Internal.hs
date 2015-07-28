-- |
-- Module      : Game.SlowChess.Move.Internal
-- Description : Movement definition for both basic and special movements
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Game.SlowChess.Move.Internal ( -- * Constructors
                                      Ply ( Move
                                          , Promotion
                                          , EnPassant
                                          , StepTwice
                                          , Castle
                                          )
                                    , Side ( Kingside, Queenside )
                                    -- * Ply manipulations
                                    , destination
                                    -- * Simple Movements
                                    , step
                                    , cast
                                    , each
                                    , lands
                                    ) where

import           Control.Monad        (mzero)

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Mask  hiding (hop)
import           Game.SlowChess.Piece

-- | Represents the changes made by in a single ply.
--
--   * When a 'Move' happens, the source becomes blank and the target is
--     overwritten with the piece moved.
data Ply =
      Move      Colour Piece Coord {-# UNPACK #-} !Coord -- ^ player piece source target
    | Promotion Colour Piece Coord Coord -- ^ player piece source target
    | EnPassant Colour Coord Coord Coord -- ^ player source target cap
    | StepTwice Colour Coord Coord Coord -- ^ player source target cap
    | Castle    Colour Side              -- ^ player side
      deriving ( Show, Eq )

-- | Move out in straight lines along the directions until either the pieces
-- choose to stop, further motion would mean stepping on a friendly unit, or
-- the last step taken removed an enemy unit.
cast :: [Direction] -> Piece -> Colour -> Board -> [Ply]
cast ds p c b = do direction <- ds
                   source    <- each c p b
                   target    <- castBasic c b direction source
                   return $ Move c p source target
{-# INLINE cast #-}

-- | Where the target square of a ply is, assuming that makes sense for the
-- type of movement.
destination :: Ply -> Maybe Coord
destination (Move      _ _ _ t) = Just t
destination (Promotion _ _ _ t) = Just t
destination (StepTwice _ _ t _) = Just t
destination (EnPassant _ _ t _) = Just t
destination _ = Nothing

-- | Does a ply's target move it to any of the places indicated in the mask?
-- Typically this is used with the mask as a filter to pick out blanks or
-- enemies.
--
-- For example, to compute where knights might land on blanks you might do:
--
-- > do p <- each Black Knight staring)
-- >    t <- jumpKnight p
-- >    lands (blanks b) t
lands :: Mask -> Coord -> [Coord]
lands m p = if p `on` m then return p else mzero
{-# INLINE lands #-}

-- | Step each piece of a kind and colour on a board in a direction, with no
-- regard for if the step is onto a valid location, returns the board.
stepAny :: Direction -> Coord -> [Coord]
stepAny d m = if hop d m == nowhere then mzero else return (hop d m)
{-# INLINE stepAny #-}

-- | Step each piece of a kind and colour on a board in a direction, landing
-- only on non-friendly squares  -- i.e. blanks or enemy units.
step :: Colour -> Board -> Direction -> Coord -> [Coord]
step c b d s = stepAny d s >>= lands (nonFriendly c b)
{-# INLINE step #-}

-- | Steps a piece over blank squares until it hits (and captures) a single
-- enemy. This is the type of motion used by rooks, bishops and queens.
castBasic :: Colour -> Board -> Direction -> Coord -> [Coord]
castBasic c b d = go 
  where go s = do candidate <- step c b d s
                  if candidate `on` blanks b
                  then candidate : go candidate
                  else if candidate `on` material (enemy c) b
                       then return candidate
                       else mzero



-- | Returns a list of each of the specified kind of piece on a mask of it's
-- own.
each :: Colour -> Piece -> Board -> [Coord]
each c p b = split (get c p b)
{-# INLINE each #-}
