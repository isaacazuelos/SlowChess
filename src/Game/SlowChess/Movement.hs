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
-- The larger piece-specific movements are made out of smaller ones — so they
-- should be sufficient to add the more complex movements later.
--
-- Here is a (potentially incomplete for now) list of the types of movements
-- not covered here.
--
-- * En Passant moves
-- * Castling
-- * Forced movement due to being in check.

module Game.SlowChess.Movement( -- * All Moves
                                 moves
                                 -- * Piece-specific Movements
                               , moveRooks
                               , moveKnights
                               , moveBishops
                               , moveQueens
                               , moveKings
                               , movePawns
                                 -- * General Movements
                               , step
                               , cast
                               ) where

import           Data.Monoid          ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
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
moveRooks c b = [N,S,E,W] >>= cast c Rook b

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
moveBishops c b = [NE,SE,NW,SW] >>= cast c Bishop b

-- | Generates all the valid movements of the queens of a colour on a board.
-- Queens can move either like rooks or like bishops — either straight or at a
-- diagonal.
moveQueens :: Colour -> Board -> [Board]
moveQueens c b = allDirections >>= cast c Queen b

-- | Generates all the valid movements of the king of a colour on a
-- board. Kings can move like queens, only they take at most 1 step in any
-- direction.
moveKings :: Colour -> Board -> [Board]
moveKings c b = allDirections >>= step c King notFriends b
  where notFriends b' = blanks b' <> material (enemy c) b'

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
--
-- Separating the computing of the moves and captures lets us reuse the same
-- functions to build both the king-like and queen-like versions of the
-- motion — exploiting the list monad.
--
-- A board is split into potential 'Movers' by 'movers' and these are rebuilt
-- into actual boards by 'rebuild'. The meat in the middle is up to the
-- particular motion type.

-- | Start our motion by breaking each piece being considered off the boards,
-- returning the mask for the position of the piece and the board.
movers :: Colour -> Piece -> Direction -> Mask -> Board -> [(Board, Mask)]
movers c p d f b = map (\m -> (removeFromBoard m, m)) hopperMasks
  where hopperMasks = split $ both (hop (rev d) f) (get c p b)
        removeFromBoard m = set c p b (get c p b `minus` m)

-- | Uses the motion of some movers to reconstitute the boards.
--
-- > rebuild m (movers m b f)) = b
rebuild :: Colour -> Piece -> Board -> Mask -> Board
rebuild c p b m = set c p b (get c p b <> m) -- use `set` so captures work

-- | Steps a piece until it cannot be stepped anymore in that direction.
castMask :: Direction -> Mask -> Mask -> [Mask]
castMask d f m = stepMask d f m ++ (stepMask d f m >>= castMask d f)

-- | Step the pieces on the second mask in a direction, but only onto squared
-- indicated by the first mask.
stepMask :: Direction -> Mask -> Mask -> [Mask]
stepMask d f m = if m' == 0 then [] else [m']
  where m' = both f (hop d m)

-- | Steps a piece over blank squares until it hit (and captures) a single
-- enemy. This is the type of motion used by rooks, bishops and queens.
cast :: Colour -> Piece -> Board -> Direction -> [Board]
cast c p b d = movers c p d (blanks b) b >>= caster
  where caster (b', m) = map (rebuild c p b') (positions m)
        positions m = casts m ++ (casts m >>= stepMask d enemies)
        casts = castMask d (blanks b)
        enemies = material (enemy c) b

-- | Steps a piece a single square in a direction, if that direction is a
-- valid position according to the passed function. This is used to allow for
-- pawn movements, since they're conditional on the type of piece on the
-- occupying square.
step :: Colour -> Piece -> (Board -> Mask) -> Board -> Direction -> [Board]
step c p f b d = movers c p d (f b) b >>= stepper
  where stepper (b', m) = map (rebuild c p b') (positions m)
        positions = stepMask d (f b)
