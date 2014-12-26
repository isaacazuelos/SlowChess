{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Game.SlowChess.Mask
-- Description : Board Representation
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Masks on a chess board indication the inclusion of exclusion of each square
-- on a board. Masks can move these squares around in certain directions, and
-- handle reasonable queries about the board.

module Game.SlowChess.Mask (
                             -- * Constructing Masks
                             Mask (Mask)
                           , fromList
                           , toList
                           ) where

import           Data.Bits
import           Data.Monoid
import           Data.Word

-- | A Mask is just using the 64 bits of a machine word to indicate which of
-- the 64 squares on a chess board are being used. This is used both to track
-- the squares being reasoned about, and also to track the positions of types
-- of pieces in a game.
--
-- Below is the mapping of bits to the squares of the board, in the board's
-- coordinates.
--
-- @
--     8 56 57 58 59 60 61 62 63
--     7 48 49 50 51 52 53 54 55
--     6 40 41 42 43 44 45 46 47
--     5 32 33 34 35 36 37 38 39
--     4 24 25 26 27 28 29 30 31
--     3 16 17 18 19 20 21 22 23
--     2 08 09 10 11 12 13 14 15
--     1 00 01 02 03 04 05 06 07
--       a  b  c  d  e  f  g  h
-- @
newtype Mask = Mask Word64 deriving (Show, Eq, Bits, Num)

-- | Masks form a monoid where the identity is an empty mask and our operation
-- preserves the occupied spaces of both operands.
--
-- @
--   1 0 0    1 0 0   1 0 0
--   0 1 0 <> 1 0 0 = 1 1 0
--   0 0 1    1 0 0   1 0 1
-- @
instance Monoid Mask where
  mempty  = 0
  mappend = (.|.)

-- | Create a mask which has only a single index set.
--
-- >                   1 0 0
-- > maskFromIndex 0 = 0 0 0
-- >                   0 0 0
maskFromIndex :: Int -> Mask
maskFromIndex i = if (i >= 0) && (i < 64)
                  then Mask (2^i)
                  else error "Index out of range for chess board."

-- | Build a mask from a list of square indexes.
--
-- >                    1 0 0
-- > fromList [0,4,8] = 0 1 0
-- >                    0 0 1
fromList :: [Int] -> Mask
fromList = foldl (\ m i -> m <> maskFromIndex i) mempty

-- | Build a list of square indicies from a mask.
--
-- @fromList . toList = toList . fromList = id@
toList :: Mask -> [Int]
toList m = filter (\i -> mempty /= m .&. maskFromIndex i) [0..63]
