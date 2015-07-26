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

module Game.SlowChess.Move.EnPassant (enPassant) where

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Piece

-- | The /en passant/ plys which follow from a current game board.
enPassant :: Game -> [Game]
enPassant g = case ply g of
                Just (StepTwice _ _ t skipped) -> do
                  let c = player g
                  let b = board g
                  source    <- each c Pawn b
                  attackDir <- forwardAttack c
                  attack    <- step c b attackDir source
                  if attack /= skipped
                    then []
                    else do let newPly = EnPassant c source skipped t
                            let wipped = squish (mask source) (mask t)
                            let newBoard = update c Pawn b wipped (mask skipped)
                            return $ next g newPly newBoard
                _ -> []
