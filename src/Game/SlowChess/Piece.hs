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

-- | Return the enemy of a colour, i.e. the other colour.
enemy :: Colour -> Colour
enemy White = Black
enemy Black = White

-- | There are different kinds of pieces on a chess board. While pieces have a
-- colour, it isn't represented here.
data Piece = Rook
           | Knight
           | Bishop
           | Queen
           | King
           | Pawn
             deriving (Show, Eq)

-- | When things move in a direction, they can be moved either up or down
-- their respective rank or file.
data Direction = Rank Step
               | File Step
                 deriving (Show, Eq)

-- | Used in @'Direction'@ to indicate the whether to step up or down along
-- the rank or file.
data Step = Up | Down deriving (Show, Eq)
