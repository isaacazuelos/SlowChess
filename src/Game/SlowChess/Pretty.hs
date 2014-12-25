-- |
-- Module      : Game.SlowChess.Pretty
-- Description : Pretty printing of game-related data.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Pretty is a tool for pretty printing the chess-related data types ---
-- things like boards, masks, and fancy Unicode representations of the piece
-- types.

module Game.SlowChess.Pretty where

import           Game.SlowChess.Piece
import           Game.SlowChess.Mask
import           Game.SlowChess.Board

class Pretty a where
  pretty :: a -> String

instance Pretty Mask where
  pretty = show

instance Pretty Board where
  pretty = show

instance Pretty Piece where
  pretty = show
