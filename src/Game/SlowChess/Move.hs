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
                           , attacks
                             -- * Piece-specific moves
                           , moveKings
                           , moveQueens
                           , moveBishops
                           , moveRooks
                           , moveKnights
                           , movePawns
                           ) where


import           Data.List                     (foldl', sortBy)
import           Data.Ord                      (comparing)

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Evaluate
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask           (Mask, squish)
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Move.Promotion
import           Game.SlowChess.Piece

-- * All movements

-- | Uses the basic movment rules to generate all a bunch of movement
-- possibilites.
moves :: Game -> [Game]
moves g = sortBy (comparing eval) $ (wrapSimple g movePawns >>= promotions)
                                 ++ wrapSimple g moveKnights
                                 ++ wrapSimple g moveBishops
                                 ++ wrapSimple g moveRooks
                                 ++ wrapSimple g moveQueens
                                 ++ wrapSimple g moveKings

-- | All the squares attackable by a colour on a board.
attacks :: Colour -> Board -> Mask
attacks c b = foldl' addTargets 0 movers
  where addTargets m f = squish m (targets (f c b))
        movers = [ moveKings
                 , moveQueens
                 , moveKnights
                 , moveBishops
                 , moveRooks
                 , movePawns
                 ]

-- * Helpers

-- | Collects the targets of some 'Move's into a mask.
{-# INLINE targets #-}
targets :: [Ply] -> Mask
targets = foldl addDestination 0
  where addDestination a m = squish a (maybe 0 mask (destination m))

-- | Wrap a simple movement up into the signature needed by even the most
-- complicated of movement types.
wrapSimple :: Game -> (Colour -> Board -> [Ply]) -> [Game]
wrapSimple g simple = map (apply g) $ simple (player g) (board g)

-- | Appply a 'Ply' to a 'Game' to get the game it makes.
-- This is /really/ stupid, and doesn't check any other rules.
{-# INLINE apply #-}
apply :: Game -> Ply -> Game
-- We only cover the constructors we need, i.e. the ones used in this module.
apply g@(Game { board = b }) m@(Move c p s t) = g'
  where g' = g { player = c
               , board  = update c p b (mask t) (mask s)
               , ply    = Just m
               }
apply g@(Game { board = b }) m@(StepTwice c s t _) = g'
  where g' = g { player = player g
               , board  = update c Pawn b (mask t) (mask s)
               , ply    = Just m
               }
apply _ p = error $ "Can't apply " ++ show p

-- * Simple Movements

-- | Generates all the valid movements of the king of a colour on a
-- board. Kings can move in any direction so long as they stay on the board
-- and don't step on any friendly material.
moveKings :: Colour -> Board -> [Ply]
moveKings c b = do source    <- each c King b
                   direction <- allDirections
                   target    <- step c b direction source
                   return $! Move c King source target

-- | Generates all the valid movements of the knights of a colour on a board.
-- Knights are capable of moving to their target squares regardless of other
-- pieces being in their way — they can jump. The only two reasons a knight
-- cannot perform one of it's 8 potential jumps are:
--
-- * The target square would not be on the board.
-- * The target square is occupied by a piece of the same colour.
moveKnights :: Colour -> Board -> [Ply]
moveKnights c b = do source <- each c Knight b
                     target <- knightHops source
                     if target `on` nonFriendly c b
                         then return $! Move c Knight source target
                         else []

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
movePawns c b =
    do source <- each c Pawn b
       let step1 = hop (forward c) source
       let step2 = hop (forward c) step1
       let stepOnce  = if step1 `on` blanks b
                          then return $! Move c Pawn source step1
                          else []
       let stepTwice = if (step2 `on` blanks b) && (source `on` homeRank c)
                          then return $! StepTwice c source step2 step1
                          else []
       let attack    = do dir <- forwardAttack c
                          let target = hop dir source
                          if target `on` material (enemy c) b
                              then return $! Move c Pawn source target
                              else []
       stepOnce ++ stepTwice ++ attack

-- | The place where pawns start.
{-# INLINE homeRank #-}
homeRank :: Colour -> Mask
homeRank White = get White Pawn starting
homeRank Black = get Black Pawn starting
