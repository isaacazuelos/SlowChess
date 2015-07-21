-- |
-- Module      : Game.SlowChess.Score
-- Description : Scoring chess games.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Scoring Chess boards.

module Game.SlowChess.Score where

import           Game.SlowChess.AI.Internal
import           Game.SlowChess.Board
import           Game.SlowChess.Game
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

-- | Score a game. Right now it's a pretty simple pice counting.
-- Values taken from https://chessprogramming.wikispaces.com/Point+Value
score :: Game -> Score
score g = fromIntegral $ sum [ 100  * count (get c Pawn b)
                             , 350  * count (get c Knight b)
                             , 350  * count (get c Bishop b)
                             , 525  * count (get c Rook b)
                             , 1000 * count (get c Queen b)
                             ]
  where c = player g
        b = board  g
