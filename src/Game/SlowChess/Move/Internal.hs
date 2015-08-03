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
                                    ) where

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Piece

-- | Represents the changes made by in a single ply.
--
--   * When a 'Move' happens, the source becomes blank and the target is
--     overwritten with the piece moved.
data Ply =
      Move      !Colour !Piece !Coord !Coord -- ^ player piece source target
    | Promotion !Colour !Piece !Coord !Coord -- ^ player piece source target
    | EnPassant !Colour !Coord !Coord !Coord -- ^ player source target cap
    | StepTwice !Colour !Coord !Coord !Coord -- ^ player source target cap
    | Castle    !Colour !Side                -- ^ player side
      deriving ( Show, Eq )

-- | Move out in straight lines along the directions until either the pieces
-- choose to stop, further motion would mean stepping on a friendly unit, or
-- the last step taken removed an enemy unit.
cast :: [Direction] -> Piece -> Colour -> Board -> [Ply]
cast ds p c b = do
    direction <- ds
    source    <- each c p b
    target    <- castBasic c b direction source
    return $! Move c p source target

-- | Where the target square of a ply is, assuming that makes sense for the
-- type of movement.
destination :: Ply -> Maybe Coord
destination (Move      _ _ _ t) = Just t
destination (Promotion _ _ _ t) = Just t
destination (StepTwice _ _ t _) = Just t
destination (EnPassant _ _ t _) = Just t
destination _ = Nothing

-- | Step each piece of a kind and colour on a board in a direction, landing
-- only on non-friendly squares  -- i.e. blanks or enemy units.
-- {-# INLINE step #-}
step :: Colour -> Board -> Direction -> Coord -> [Coord]
step c b d s = [hop d s | hop d s `on` nonFriendly c b]

-- | Steps a piece over blank squares until it hits (and captures) a single
-- enemy. This is the type of motion used by rooks, bishops and queens.
{-# INLINE castBasic #-}
castBasic :: Colour -> Board -> Direction -> Coord -> [Coord]
castBasic c b d = go
  where go s
          | candidate `on` blanks b             = candidate : go candidate
          | candidate `on` material (enemy c) b = [candidate]
          | otherwise = []
            where candidate = hop d s

-- | Returns a list of each of the specified kind of piece on a mask of it's
-- own.
each :: Colour -> Piece -> Board -> [Coord]
each c p b = split (get c p b)
