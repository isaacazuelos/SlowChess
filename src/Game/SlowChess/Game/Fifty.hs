-- |
-- Module      : Game.SlowChess.Fifty
-- Description : The threefold rule
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
--

module Game.SlowChess.Game.Fifty (fiftyMove) where

import Game.SlowChess.Move.Internal
import Game.SlowChess.Game.Internal
import Game.SlowChess.Piece

fiftyMove :: Rule
fiftyMove g = return g { fifty = updateFifty g }

-- | Increment if the last ply is not a pawn moving or a capture, otherwise
-- it's reset to 0.
updateFifty :: Game -> Int
updateFifty g = case ply g of
                    Nothing                -> 0
                    Just (Move _ Pawn _ _) -> 0
                    _                      -> 1 + fifty g
