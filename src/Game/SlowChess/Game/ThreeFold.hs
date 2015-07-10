-- |
-- Module      : Game.SlowChess.ThreeFold
-- Description : The threefold rule
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A game player can claim a draw if the current position of the board has
-- been seen twice before, and future of those moves are also the same.

module Game.SlowChess.Game.ThreeFold (threeFold) where

import Game.SlowChess.Game.Internal

-- Note that we can't just check the board, since the availability of /en
-- passant/ moves depends on the previous moves.

threeFold :: Rule
threeFold g = return $ if 2 == length (filter (same g) (history g))
                         then g { drawStatus = Claimable }
                         else g

-- | Games are the same (for the purpose of this rule) if:
--
same :: Game -> Game -> Bool
same a b = player a == player b
            && board a == board b
            -- Since the futures are generated by the same functions, they
            -- ought to produce possible futures in the same order.
            && _future a == _future b
            && options a == options b
