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

module Game.SlowChess.Mask ( Mask (Mask)
                           ) where

import           Data.Bits
import           Data.Monoid
import           Data.Word

-- | A Mask is just using the 64 bits of a machine word to indicate which of
-- the 64 squares on a chess board are being used. This is used both to track
-- the squares being reasoned about, and also to track the positions of types
-- of pieces in a game.
--
-- @
--     1 00 01 02 03 04 05 06 07
--     2 08 09 10 11 12 13 14 15
--     3 16 17 18 19 20 21 22 23
--     4 24 25 26 27 28 29 30 31
--     5 32 33 34 35 36 37 38 39
--     6 40 41 42 43 44 45 46 47
--     7 48 49 50 51 52 53 54 55
--     8 56 57 58 59 60 61 62 63
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
