-- |
-- Module      : Game.SlowChess.Fifty
-- Description : The threefold rule
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
--

module Game.SlowChess.Game.Fifty (fifty) where

import Game.SlowChess.Move.Internal
import Game.SlowChess.Game.Internal
import Game.SlowChess.Piece

-- | The fifty move rule. A game can be drawn after 50 moves where nothing
-- is captured or no pawns move.
fifty :: Rule
fifty g = return g { fiftyStatus = updateFifty g }

-- | Increment if the last ply is not a pawn moving or a capture, otherwise
-- it's reset to 0.
updateFifty :: Game -> Int
updateFifty g = case ply g of
                    Nothing                -> 0
                    Just (Move _ Pawn _ _) -> 0
                    _                      -> 1 + fiftyStatus g
