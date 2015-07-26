-- |
-- Module      : Game.SlowChess.ThreeFold
-- Description : The threefold rule
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A game player can claim a draw if the current position of the board has
-- been seen twice before, and future of those moves are also the same.
--
-- This isn't set up to be called as part of the tree search, since the
-- AI will never claim a draw.

module Game.SlowChess.Game.ThreeFold (threeFoldRule) where

import           Data.List                    (groupBy)
import           Data.Maybe                   (mapMaybe)

import           Game.SlowChess.Game.Internal

-- Note that we can't just check the board, since the availability of /en
-- passant/ moves depends on the previous moves.

-- | The threefold move rule. A draw can be claimed if the game has been in
-- the same position (with the same posible moves) three times. Here we expect
-- the complete history to be passed in.
threeFoldRule :: [Game] -> Bool
threeFoldRule gs = 3 <= maximum (map length (groupBy same gs))

-- | Games are the same (f(r the purpose of this rule) if:
--
-- 1. The boards are the same.
-- 2. The allowable moves are the same.
-- 3. The game's castle status hasn't changed.
--
-- Note that the allowable moves will be the same if
same :: Game -> Game -> Bool
same a b = board a == board b
            && castleOptions a == castleOptions b
            && moves a == moves b
  where moves = mapMaybe ply . future
