-- |
-- Module      : Game.SlowChess.EnPassant
-- Description : Castling related functions
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- The rules are a little weird, but here's my take on what
-- <https://en.wikipedia.org/wiki/En_passant Wikipedia> says.
--
-- After a pawn steps twice, if it could be attacked by another pawn had it
-- stepped once, then it can be captured as if it had stepped once --- the
-- attacker moves to the square stepped over but the pawn is still captured.

module Game.SlowChess.Move.EnPassant where

import           Control.Monad                (mzero)

import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Piece

-- | The /en passant/ plys which follow from a current game board.
enPassant :: Game -> [Ply]
-- This is kind of an abuse of pattern matching, I think.
enPassant (Game { player  = c
                , board   = b
                , history = (_, StepTwice _ _ t skipped):_
                }) = do attackDir <- forwardAttack c
                        source    <- each c Pawn b
                        attack    <- step c b attackDir source
                        -- valid attacks are those which land on the skipped
                        -- square.
                        target    <- if attack == skipped
                                        then return attack
                                        else mzero
                        return $ EnPassant c source target t
enPassant _ = []
