-- |
-- Module      : Game.SlowChess.Movement
-- Description : Move generation for basic movements.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Move generation for chess pieces.
--
-- Move generation in Chess can be pretty complicated. This module handles the
-- basic types of movements that apply to the different kinds of pieces in
-- their typical cases. Some allowed kinds of movement can not be handled
-- here, as they require greater knowledge of the state of the game than just
-- the current board. Since no notion of a /game/ exists yet, we can't work on
-- that.
--
-- Here are the types of movements not covered here.
--
-- * En Passant moves
-- * Castling
-- * Forced movement due to being in check.
--
-- These /larger/ movements are made out of smaller ones.

module Game.SlowChess.Movement {-
                               ( -- * Move
                                 moves
                                 -- * Piece-specific Movements
                               , moveRooks
                               , moveKnights
                               , moveBishops
                               , moveQueens
                               , moveKings
                               , movePawns
                                 -- * General Movements
                               , move
                               ) -} where

import           Game.SlowChess.Mask
import           Game.SlowChess.Board
import           Game.SlowChess.Piece

-- | Generates all the non-special movements that the pieces of a colour can
-- make.
moves :: Colour -> Board -> [Board]
moves c b = concat [ moveRooks   c b
                   , moveKnights c b
                   , moveBishops c b
                   , moveQueens  c b
                   , moveKings   c b
                   , movePawns   c b
                   ]

-- | Generates all the valid boards which follow from movements of the rooks
-- of a colour on a board.  Rooks move out in straight lines along their rank
-- or file until they choose to stop, further motion would mean stepping on a
-- friendly unit, or the last step taken removed an enemy unit.
moveRooks :: Colour -> Board -> [Board]
moveRooks = undefined

-- | Generates all the valid movements of the knights of a colour on a board.
-- Knights are capable of moving to their target squares regardless of other
-- pieces being in their way — they can jump. The only two reasons a knight
-- cannot perform one of it's 8 potential jumps are:
--
-- * The target square would not be on the board.
-- * The target square is occupied by a piece of the same colour.
moveKnights :: Colour -> Board -> [Board]
moveKnights = undefined

-- | Generates all the valid movements of the bishops of a colour on a board.
-- Bishops move diagonally, under the same conditions as rooks.
moveBishops :: Colour -> Board -> [Board]
moveBishops = undefined

-- | Generates all the valid movements of the queens of a colour on a board.
-- Queens can move either like rooks or like bishops — either straight or at a
-- diagonal.
moveQueens :: Colour -> Board -> [Board]
moveQueens = undefined

-- | Generates all the valid movements of the king of a colour on a
-- board. Kings can move like queens, only they take at most 1 step in any
-- direction.
moveKings :: Colour -> Board -> [Board]
moveKings = undefined

-- | Generates *some* the valid movements of the pawns of a colour on a board.
-- Pawn motion is the most complicated. Below are the rules governing pawn
-- movement as implemented here. Some more special rules that require more
-- information about game state and history are elsewhere.
--
-- * Pawns can only move forward, i.e. along their files away from the rank
--   that the king of the same colour started on.
--
-- * Pawns can capture if the squares diagonally forward are occupied by an
--   enemy piece.
--
-- * Pawns can move straight forward onto a blank square.
--
-- * Pawns on their starting rank can move directly forward either one or two
--   squares. Since they cannot move backwards, if a pawn is on the starting
--   rank for it's colour, than it hasn't moved.
movePawns :: Colour -> Board -> [Board]
movePawns = undefined

-- * General Movements

-- | Move all pieces of a kind and colour in a direction by a single square.
move :: Direction -> Colour -> Piece -> Board -> Board
move d c p b = set c p b (hop d (get c p b))

