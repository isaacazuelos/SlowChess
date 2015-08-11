-- |
-- Module      : Game.SlowChess.Evaluate
-- Description : Scoring chess games.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Chess game evaluation. Evaluating a game assigns the game a score based
-- on how well the current player is doing. The goal is to get as good an
-- estimate as possible /without/ looking ahead.

module Game.SlowChess.Evaluate (eval) where

import           Game.SlowChess.AI.Internal
import           Game.SlowChess.Board
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

-- | Evaluate a game. Right now it's a pretty simple pice counting.
-- Values taken from https://chessprogramming.wikispaces.com/Point+Value
eval :: Game -> Score
eval g -- if checkmate g then 1/0 else countPieces g
    | checkmate g = 1/0
    | check     g = 2500
    | draw      g = -500
    | otherwise   = countPieces g
-- | Count the number of pieces, and give each a score. See 'eval' for
-- details.
countPieces :: Game -> Score
countPieces g = fromIntegral counts
  where c = player g
        b = board  g
        counts = (100  * count (get c Pawn b))
               + (350  * count (get c Knight b))
               + (350  * count (get c Bishop b))
               + (525  * count (get c Rook b))
               + (1000 * count (get c Queen b))
