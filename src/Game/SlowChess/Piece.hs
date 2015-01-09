-- |
-- Module      : Game.SlowChess.Piece
-- Description : Piece types and other chess jargon.
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
-- their respective rank or file, or some combination of both. For the sake of
-- brevity, I've used the cardinal directions (where A8 is the NW corner.)
data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

-- | Reverse a direction.
--
-- > rev . rev = id
rev :: Direction -> Direction
rev N  = S
rev NE = SW
rev E  = W
rev SE = NW
rev S  = N
rev SW = NE
rev W  = E
rev NW = SE

-- | All of the directions.
allDirections :: [Direction]
allDirections = [N, NE, E, SE, S, SW, W, NW]
