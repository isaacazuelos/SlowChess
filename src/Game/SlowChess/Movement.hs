-- |
-- Module      : Game.SlowChess.Movement
-- Description : Move generation for basic movements.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Simple move generation for chess pieces.
--
-- Move generation in Chess can be pretty complicated. This module handles the
-- basic types of movements that apply to the different kinds of pieces in
-- their typical cases.
--
-- Some allowed kinds of movement require greater knowledge of the state of
-- the game than just the current board. For more information on this issue,
-- and how it's being worked around, see the note in
-- 'Game.SlowChess.Game.Internal'.
--
-- Here is a (potentially incomplete for now) list of the types of movements
-- not covered here yet, which are referred to as the /special/ movements..
--
-- * En Passant moves
-- * Castling
-- * Forced movement due to being in check.

module Game.SlowChess.Movement ( -- * All Moves
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

import           Data.Monoid           ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

-- | Generates all the non-special movements for the pieces of a colour.
moves :: Colour -> Board -> [Board]
moves c b = concat [ moveRooks   c b
                   , moveKnights c b
                   , moveBishops c b
                   , moveQueens  c b
                   , moveKings   c b
                   , movePawns   c b
                   ]

-- | Generates all the valid boards which follow from movements of the rooks
-- of a colour on a board. Rooks move out in straight lines along their rank
-- or file until they choose to stop, further motion would mean stepping on a
-- friendly unit, or the last step taken removed an enemy unit.
moveRooks :: Colour -> Board -> [Board]
moveRooks c b = [N, S, E, W] >>= cast c Rook b

-- | Generates all the valid movements of the knights of a colour on a board.
-- Knights are capable of moving to their target squares regardless of other
-- pieces being in their way — they can jump. The only two reasons a knight
-- cannot perform one of it's 8 potential jumps are:
--
-- * The target square would not be on the board.
-- * The target square is occupied by a piece of the same colour.
moveKnights :: Colour -> Board -> [Board]
moveKnights c b = do (b', knight) <- splitOff (knights b) c Knight b
                     movedKnight  <- knightLandings knight
                     if 0 == both movedKnight (blanks b <> material (enemy c) b)
                       then []
                       else return $ rebuild c Knight b' movedKnight

-- | For a knight at a coordinate, it gives all coordinates that it could jump
-- to. This does filter out jumps that would move the piece off the board.
knightLandings :: Mask -> [Mask]
knightLandings m = filter (/= 0) $ map (\ f -> f m) [ hop N . hop N . hop E
                                                    , hop E . hop E . hop N
                                                    , hop E . hop E . hop S
                                                    , hop S . hop S . hop E
                                                    , hop S . hop S . hop W
                                                    , hop W . hop W . hop S
                                                    , hop W . hop W . hop N
                                                    , hop N . hop N . hop W
                                                    ]

-- | Generates all the valid movements of the bishops of a colour on a board.
-- Bishops move diagonally, under the same conditions as rooks.
moveBishops :: Colour -> Board -> [Board]
moveBishops c b = [NE, SE, NW, SW] >>= cast c Bishop b

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
movePawns :: Colour -> Board -> [Board]
movePawns c b = concat [ stepForward
                       , capture
                       , stepTwice
                       ]
  where fwd = forward c
        stepForward = step c Pawn (blanks) b fwd
        capture = forwardAttack c >>= step c Pawn (material (enemy c)) b
        stepTwice = map (uncurry $ rebuild c Pawn) steppedTwice
        twiceSteppers = splitOff (canStepTwice c b) c Pawn b -- :: [(Board, Mask)]
        hopTwice = hop fwd . hop fwd
        steppedTwice = map (\ (b', m) ->(b', hopTwice m)) twiceSteppers

-- | The pieces that can move two squares forward, i.e. the pieces with two
-- blanks in front of them on the starting rank for their colour.
canStepTwice :: Colour -> Board -> Mask
canStepTwice c b = (get c Pawn b)
                   `both` (hop back (blanks b))
                   `both` (hop back (hop back (blanks b)))
                   `both` (startingRank c)
  where back = rev (forward c)

-- | The squares a piece can land on from the staring rank.
startingRank :: Colour -> Mask
startingRank White = fromList [8..15]
startingRank Black = fromList [48..55]

-- | The /forward/ direction as used by pawn motions.
forward :: Colour -> Direction
forward White = N
forward Black = S

-- | The directions the pawns of a colour can attack.
forwardAttack :: Colour -> [Direction]
forwardAttack White = [NE, NW]
forwardAttack Black = [SE, SW]

-- * General Movements
--
-- Separating the computing of the moves and captures lets us reuse the same
-- functions to build both the king-like and queen-like versions of the
-- motion — exploiting the list monad.
--
-- A board is split into potential 'Movers' by 'movers' and these are rebuilt
-- into actual boards by 'rebuild'. The meat in the middle is up to the
-- particular motion type.

-- | Pieces on the first filter mask are broken off the board.
splitOff :: Mask -> Colour -> Piece -> Board -> [(Board, Mask)]
splitOff f c p b = map takeOff (split $ both f (get c p b))
  where takeOff m = (set c p b (get c p b `minus` m), m)

-- | Start our motion by breaking each piece being considered off the boards,
-- returning the mask for the position of the piece and the board.
movers :: Mask -> Direction -> Colour -> Piece -> Board -> [(Board, Mask)]
movers f d = splitOff (hop (rev d) f)

-- | Uses the motion of some movers to reconstitute the boards.
--
-- > rebuild m (movers m b f)) = b
rebuild :: Colour -> Piece -> Board -> Mask -> Board
rebuild c p b m = set c p b (get c p b <> m) -- use `set` so captures work

-- | Steps a piece until it cannot be stepped anymore in that direction, or it
-- has move the number of steps indicated.
castMask :: Direction -> Mask -> Mask -> [Mask]
castMask d f m = stepMask d f m ++ (stepMask d f m >>= castMask d f)

-- | Step the pieces on the second mask in a direction, but only onto squared
-- indicated by the first mask.
stepMask :: Direction -> Mask -> Mask -> [Mask]
stepMask d f m = if m' == 0 then [] else [m']
  where m' = both f (hop d m)

-- | Steps a piece over blank squares until it hits (and captures) a single
-- enemy. This is the type of motion used by rooks, bishops and queens.
cast :: Colour -> Piece -> Board -> Direction -> [Board]
cast c p b d = movers (blanks b) d c p b >>= caster
  where caster (b', m) = map (rebuild c p b') (positions m)
        positions m = casts m ++ (casts m >>= stepMask d enemies)
        casts = castMask d (blanks b)
        enemies = material (enemy c) b

-- | Steps a piece a single square in a direction, if that direction is a
-- valid position according to the passed function. This is used to allow for
-- pawn movements, since they're conditional on the type of piece on the
-- occupying square.
step :: Colour -> Piece -> (Board -> Mask) -> Board -> Direction -> [Board]
step c p f b d = movers (f b) d c p b >>= stepper
  where stepper (b', m) = map (rebuild c p b') (positions m)
        positions = stepMask d (f b)
