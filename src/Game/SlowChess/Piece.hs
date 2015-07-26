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
{-# INLINE enemy #-}

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
-- brevity, the cardinal directions are used (where A8 is the NW corner.)
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
{-# INLINE rev #-}

-- | All of the directions in a list.
allDirections :: [Direction]
allDirections = [N, NE, E, SE, S, SW, W, NW]

-- | The /forward/ direction as used by pawn motions.
forward :: Colour -> Direction
forward White = N
forward Black = S
{-# INLINE forward #-}

-- | The directions the pawns of a colour can attack.
forwardAttack :: Colour -> [Direction]
forwardAttack White = [NE, NW]
forwardAttack Black = [SE, SW]
{-# INLINE forwardAttack #-}

-- | Colour-relative board sides, used for knowing which side a castle move
-- was done to.
data Side = Kingside | Queenside deriving ( Show, Eq )
