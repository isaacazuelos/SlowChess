-- |
-- Module      : Game.SlowChess.Piece
-- Description : Board Representation
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Basic chess terms.

module Game.SlowChess.Piece where

-- | There are two colours used in Chess, white and black. They have names and
-- are not equal.
data Colour = White | Black deriving (Show, Eq)

-- | There are different kinds of pieces on a chess board, which are coloured.
data Piece = Rook   Colour
           | Knight Colour
           | Bishop Colour
           | Queen  Colour
           | King   Colour
           | Pawn   Colour
             deriving (Show, Eq)
