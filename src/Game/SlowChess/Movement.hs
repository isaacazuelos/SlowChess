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
                                 moveGen
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

import           Data.Monoid           ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

import           Game.SlowChess.Pretty (pprint)

-- | Generates all the non-special movements that the pieces of a colour can
-- make.
moveGen :: Colour -> Board -> [Board]
moveGen c b = concat [ moveRooks   c b
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
moveRooks c b = undefined -- [N,S,E,W] >>= (\ d -> cast c Rook d b)

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

-- -- | Generates all the valid movements of the king of a colour on a
-- -- board. Kings can move like queens, only they take at most 1 step in any
-- -- direction.
moveKings :: Colour -> Board -> [Board]
moveKings c b = undefined -- allDirections >>= (hops c King b nonFriendly)
--   where nonFriendly b' = (material (enemy c) b') <> (blanks b')

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

-- | Motions on the board can happen to pieces of a colour in a direction
-- under a certain tile predicate. 
data Motion = Motion { colour    :: Colour         -- ^ test
                     , piece     :: Piece
                     , direction :: Direction
                     , condition :: Board -> Mask
                     }

type Mover = (Mask, Board)

-- | Start our motion 
movers :: Motion -> Board -> [Mover]
movers (Motion c p d f) b = map (\m -> (m, removeFromBoard m)) hopperMasks
  where hopperMasks = split $ both (hop (rev d) (f b)) (get c p b)
        removeFromBoard m = set c p b ((get c p b) `minus` m)

-- | Uses the motion of some movers to reconstitute the boards.
--
-- > rebuild m (movers m b f)) = b
rebuild :: Motion -> [Mover] -> [Board]
rebuild (Motion c p _ _) ms = map (\ (m, b) -> apply c p b (<> m)) ms

-- | Move a motion's piece along it's direction until it's no longer able to
-- move without violating the predicate. 
cast :: Motion -> Mover -> [Mover]
cast = undefined
