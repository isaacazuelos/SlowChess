-- |
-- Module      : Game.SlowChess.Fifty
-- Description : The threefold rule
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
--

module Game.SlowChess.Game.Fifty (fiftyMoveRule) where

import Game.SlowChess.Move.Internal
import Game.SlowChess.Game.Internal
import Game.SlowChess.Piece

-- | The fifty move rule. A game can be drawn after 50 moves where nothing
-- is captured or no pawns move.
fiftyMoveRule :: Game -> Game
fiftyMoveRule g = if updated < 50 then g' else setDraw g' True
  where g' = setFifty g updated
        updated = case ply g of
                    Just (Move _ Pawn _ _) -> 0
                    _                      -> 1 + fifty g
