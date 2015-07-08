-- |
-- Module      : Game.SlowChess.Move
-- Description : Movement definitions.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Move generation for chess pieces.
--
-- Move generation in Chess can be pretty complicated. Some allowed kinds of
-- movement require greater knowledge of the state of the game than just the
-- current board. For more information on this issue, and how it's being
-- worked around, see the note in 'Game.SlowChess.Game.Internal'.
--
-- This module contains simple movements. The special movements are in their
-- own modules. Tools for writing movements can be found in
-- 'Game.SlowChess.Move.Internal'.

module Game.SlowChess.Move ( -- * Move generation
                             Ply
                           , moves
                             -- * Helpful functions for working with moves
                           , targets
                           , wrapSimple
                           , doPly
                            -- * Piece-specific moves
                           , moveKings
                           , moveQueens
                           , moveBishops
                           , moveRooks
                           , moveKnights
                           , movePawns
                           ) where

import           Data.Monoid                   ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask           (Mask)
import           Game.SlowChess.Move.Castle
import           Game.SlowChess.Move.EnPassant
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Move.Promotion
import           Game.SlowChess.Piece

-- * All movements

-- | Return all plys which are responses to a game. This /does not/ filter
-- out plys that would put the player in check.
moves :: Game -> [Ply]
moves g = [ castle
          , enPassant
          , wrapSimple moveQueens
          , wrapSimple moveKnights
          , wrapSimple moveRooks
          , wrapSimple moveBishops
          , wrapSimple moveKings
          , promotions . wrapSimple movePawns
          ] >>= ($ g)

-- * Helpers

-- | Collects the targets of some 'Move's into a mask.
targets :: [Ply] -> Mask
targets = foldl (\ a m -> a <> maybe 0 mask (destination m)) 0

-- | Wrap a simple movement up into the signature needed by even the most
-- complicated of movement types.
wrapSimple :: (Colour -> Board -> [Ply]) -> Game -> [Ply]
wrapSimple simple g = simple (player g) (board g)

-- | Apply the 'Ply' to the 'Board' to get it's affect on the board.
doPly :: Ply -> Board -> Board
doPly (Castle c s)        b = blindlyCastle c b s
doPly (Move c p s t)      b = update c p    (wipe b (mask s)) (<> mask t)
doPly (Cap c p s _ t)     b = update c p    (wipe b (mask s)) (<> mask t)
doPly (StepTwice c s t _) b = update c Pawn (wipe b (mask s)) (<> mask t)
doPly (Promotion c p s t) b = update c p    (wipe b (mask s)) (<> mask t)
doPly (EnPassant c s t e) b = update c Pawn boardWithRemovals (<> mask t)
  where boardWithRemovals = wipe b (mask s <> mask e)

-- * Simple Movements

-- | Generates all the valid movements of the king of a colour on a
-- board. Kings can move in any direction so long as they stay on the board
-- and don't step on any friendly material.
moveKings :: Colour -> Board -> [Ply]
moveKings c b = do direction <- allDirections
                   source    <- each c King b
                   target    <- step c b direction source
                   return $ Move c King source target

-- | Generates all the valid movements of the knights of a colour on a board.
-- Knights are capable of moving to their target squares regardless of other
-- pieces being in their way — they can jump. The only two reasons a knight
-- cannot perform one of it's 8 potential jumps are:
--
-- * The target square would not be on the board.
-- * The target square is occupied by a piece of the same colour.
moveKnights :: Colour -> Board -> [Ply]
moveKnights c b = do source    <- each c Knight b
                     candidate <- knightHops source
                     target    <- lands (nonFriendly c b) candidate
                     return $ Move c Knight source target

-- | For a knight at a coordinate, it gives all coordinates that it could jump
-- to --- even if that means jumping off the board.
knightHops ::  Coord -> [Coord]
knightHops m = map (`hopBy` m) dirs
 where dirs = [(N,E), (E,N), (E,S), (S,E), (S,W), (W,S), (W,N), (N,W)]
       hopBy (a,b) = hop a . hop a . hop b

-- | Generates all the valid boards which follow from movements of the rooks
-- of a colour on a board. Rooks move by being cast out along blanks, stopping
-- either where they choose, after capturing a single enemy unit, before they
-- run off the board, or before they hit a friendly unit.
moveRooks :: Colour -> Board -> [Ply]
moveRooks = cast [N, S, E, W] Rook

-- | Generates all the valid movements of the bishops of a colour on a board.
-- Bishops move diagonally, under the same conditions as rooks.
moveBishops :: Colour -> Board -> [Ply]
moveBishops = cast [NE, SE, NW, SW] Bishop

-- | Generates all the valid movements of the queens of a colour on a board.
-- Queens can move either like rooks or like bishops — either straight or at a
-- diagonal.
moveQueens :: Colour -> Board -> [Ply]
moveQueens = cast allDirections Queen

-- | Generates *some* the valid movements of the pawns of a colour on a board.
-- Pawn motion is the most complicated. Below are the rules governing pawn
-- movement as implemented here. Some more special rules that require more
-- information about game state and history are elsewhere in this module.
--
-- * Pawns can only move /forward/, i.e. along their files away from the rank
--   that the king of the same colour started on.
--
-- * Pawns can capture if the squares diagonally forward are occupied by an
--   enemy piece.
--
-- * Pawns can move straight forward onto a blank square.
--
-- * Pawns on their starting rank can move directly forward either one or two
--   squares. Since they cannot move backwards, if a pawn is on the starting
--   rank for its colour, then it hasn't moved.
movePawns :: Colour -> Board -> [Ply]
movePawns c b = captures ++ stepOnce ++ stepTwice
  where captures  = do attackDir <- forwardAttack c
                       source    <- each c Pawn b
                       attacks   <- step c b attackDir source
                       target    <- lands (material (enemy c) b) attacks
                       return $ Move c Pawn source target
        stepOnce  = do source <- each c Pawn b
                       target <- step c b (forward c) source
                       return $ Move c Pawn source target
        stepTwice = do source <- each c Pawn b
                       step1  <- step c b (forward c) source
                       target <- step c b (forward c) step1
                       if (source `on` homeRank c)
                           && (step1  `on` blanks b)
                           && (target `on` blanks b)
                         then return $ StepTwice c source target step1
                         else []

-- | The place where pawns start.
homeRank :: Colour -> Mask
homeRank White = get White Pawn starting
homeRank Black = get Black Pawn starting
