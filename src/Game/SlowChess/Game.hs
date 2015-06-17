-- |
-- Module      : Game.SlowChess.Game
-- Description : A game
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model.

module Game.SlowChess.Game where

import Game.SlowChess.Piece
import Game.SlowChess.Board
import Game.SlowChess.Game.Internal

new :: Game
new = Game White Normal starting []

