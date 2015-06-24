-- |
-- Module      : Game.SlowChess.Move
-- Description : Movement definition for both basic and special movements.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Game.SlowChess.Move ( Ply (Move)
                             -- * Piece-specific Plys
                           , moveKings
                           , moveKnights
                           , moveRooks
                           , moveBishops
                           , moveQueens
                           , movePawns
                             -- * Working with Plys
                           , apply
                           ) where

import           Control.Monad

import           Data.Monoid          ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

-- | Represents the changes made by in a single ply.
--
--   * When a 'Move' happens, the source becomes blank and the target is
--     overwritten with the piece moved.
data Ply = Move Colour Piece Mask Mask deriving (Show, Eq)

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
knightHops ::  Mask -> [Mask]
knightHops m = map (`hopBy` m) dirs
 where dirs = [(N,E), (E,N), (E,S), (S,E), (S,W), (W,S), (W,N), (N,W)]
       hopBy (a,b) = hop a . hop a . hop b

-- | Move out in straight lines along the directions until either the pieces
-- choose to stop, further motion would mean stepping on a friendly unit, or œthe
-- last step taken removed an enemy unit.
moveByCasting :: [Direction] -> Piece -> Colour -> Board -> [Ply]
moveByCasting ds p c b = do direction <- ds
                            source    <- each c p b
                            target    <- cast c b direction source
                            return $ Move c p source target

-- | Generates all the valid boards which follow from movements of the rooks
-- of a colour on a board. Rooks move by being cast out along blanks, stopping
-- either where they choose, after capturing a single enemy unit, before they
-- run off the board, or before they hit a friendly unit.
moveRooks :: Colour -> Board -> [Ply]
moveRooks = moveByCasting [N, S, E, W] Rook

-- | Generates all the valid movements of the bishops of a colour on a board.
-- Bishops move diagonally, under the same conditions as rooks.
moveBishops :: Colour -> Board -> [Ply]
moveBishops = moveByCasting [NE, SE, NW, SW] Bishop

-- | Generates all the valid movements of the queens of a colour on a board.
-- Queens can move either like rooks or like bishops — either straight or at a
-- diagonal.
moveQueens :: Colour -> Board -> [Ply]
moveQueens = moveByCasting allDirections Queen

-- TODO: update 'elsewhere' to location
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
                       return $ Move c Pawn source target

-- | The /forward/ direction as used by pawn motions.
forward :: Colour -> Direction
forward White = N
forward Black = S

-- | The directions the pawns of a colour can attack.
forwardAttack :: Colour -> [Direction]
forwardAttack White = [NE, NW]
forwardAttack Black = [SE, SW]

-- | Does a ply's target move it to any of the places indicated in the mask?
-- Typically this is used with the mask as a filter for to pick out blanks or
-- enemies.
--
-- For example, to compute where knights might land on blanks you might do:
--
-- > do p <- each Black Knight staring)
-- >    t <- jumpKnight p
-- >    lands (blanks b) t
lands :: Mask -> Mask -> [Mask]
lands m p = if p `on` m then return p else mzero

-- | Are the tiles of the first mask a subset of the second mask's tiles?
-- Returns False if the first mask if 0, since that already means the piece is
-- off the board.
on :: Mask -> Mask -> Bool
on s m = s /= 0 && (s <> m) == m

-- | Step each piece of a kind and colour on a board in a direction, with no
-- regard for if the step is onto a valid location, returns the board.
stepAny :: Direction -> Mask -> [Mask]
stepAny d m = if (hop d m) == 0 then mzero else return (hop d m)

-- | Step each piece of a kind and colour on a board in a direction, landing
-- only on non-friendly squares  --- i.e. blanks or enemy units.
step :: Colour -> Board -> Direction -> Mask -> [Mask]
step c b d s = stepAny d s >>= lands (nonFriendly c b)

-- | Steps a piece over blank squares until it hits (and captures) a single
-- enemy. This is the type of motion used by rooks, bishops and queens.
cast :: Colour -> Board -> Direction -> Mask -> [Mask]
cast c b d s = do candidate <- step c b d s
                  if candidate `on` (blanks b)
                    then candidate:(cast c b d candidate)
                    else if candidate `on` (material (enemy c) b)
                         then return candidate
                         else mzero

-- | Change a board by applying the move from a ply.
apply :: Board -> Ply -> Board
apply b (Move c p s t) = update c p b (\ m -> t <> (m `minus` s))

-- | Returns a list of each of/ the specified kind of piece on a mask of it's
-- own.
each :: Colour -> Piece -> Board -> [Mask]
each c p b = split (get c p b)
