-- |
-- Module      : Game.SlowChess.Board
-- Description : Board Representation
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess board is just the tiles of the board 8x8 board, and which kind of
-- piece they're occupied by.

module Game.SlowChess.Board where

import           Game.SlowChess.Mask

-- * Board

-- | A chess board is represented as the state of the pieces of both sides.
data Board = Board { white :: Side
                   , black :: Side
                   } deriving (Show, Eq)


-- | There are two sides, each with the 'Mask's which store the state of the
-- different types of pieces belonging to that side.
data Side  = Side { pawns   :: Mask
                  , rooks   :: Mask
                  , bishops :: Mask
                  , knights :: Mask
                  , kings   :: Mask
                  , queens  :: Mask
                  } deriving (Show, Eq)

-- | A blank board is the board with no pieces on it.
blank :: Board
blank = Board { white = Side 0 0 0 0 0 0
              , black = Side 0 0 0 0 0 0
              }
