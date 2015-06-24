-- |
-- Module      : Game.SlowChess.Game
-- Description : A game
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model.

module Game.SlowChess.Game where

import           Game.SlowChess.Board
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Piece

-- | Tracks all the state around a game of chess.
new :: Game
new = Game { player    = White
           , board     = starting
           , condition = Normal
           , castle    = Castle True True
           , history   = []
           }

