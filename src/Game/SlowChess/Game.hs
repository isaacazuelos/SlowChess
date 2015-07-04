-- |
-- Module      : Game.SlowChess.Game
-- Description : A game
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model.

module Game.SlowChess.Game ( new, check, checkmate ) where

import           Game.SlowChess.Board
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Piece

-- | Tracks all the state around a game of chess.
new :: Game
new = Game White starting []

-- * Special

-- | Is the a game in check? A game is in check if the current player's king
-- can be attacked if they weren't to move at all.
check :: Game -> Bool
check g = any (capturesKing g) (moves $ g {player = enemy (player g)})

-- | Is the game won? A game is in checkmate if the current player cannot
-- make a move which does not yeild check.
checkmate :: Game -> Bool
checkmate = undefined

-- | If there's a king before and no king after applying the ply, we can
-- assume it was captured.
capturesKing :: Game -> Ply -> Bool
capturesKing (Game c b _) p = (get c King b /= 0)
    && (get c King (apply p b) == 0)
