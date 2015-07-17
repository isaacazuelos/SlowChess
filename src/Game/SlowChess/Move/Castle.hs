-- |
-- Module      : Game.SlowChess.Castle
-- Description : Castling related functions
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- When the king and the a rook have not moved, they can castle --- moving the
-- king two squareBefores towards a rook on the player's first rank, then moving the
-- rook to the squareBefore over which the king crossed.
--
-- Castling may only be done if:
--
-- 1. The king has never moved, the rook involved has never moved.
--
-- 2. The squareBefores between the king and the rook involved are unoccupied
--
-- 3. The king is not in check, and the king does not cross over or end on a
--    squareBefore in which it would be in check.
--
-- This module only checks teh first two rules and assumes that the board
-- started in the typical starting position.

module Game.SlowChess.Move.Castle ( castle, allOptions ) where

import           Data.List                    (delete)
import           Data.Monoid                  ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Mask          hiding (fromList)
import           Game.SlowChess.Piece

import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move.Internal

-- | Returns the mostly-legal castling moves for a game. This even includes
-- plys that would put the player in check. Since you /can't/ put youself into
-- check, that's ensured elsewhere.
castle :: Rule
castle g = do s <- [Kingside, Queenside]
              let c = player g
              let p = Castle c s
              if canCastle g s
                  then return
                    (next g p (blindlyCastle c (board g) s))
                        { castleStatus = delete (c, s) (castleStatus g) }
                  else []

-- | All possible castling options
allOptions :: [(Colour, Side)]
allOptions = [(c, s) | c <- [White, Black], s <- [Queenside, Kingside]]

-- | Do the actions to castle to a board, but /without/ any of the checks.
-- This just blindly removes the pieces, and puts a king and rook where
-- they ought to land.
blindlyCastle :: Colour -> Board -> Side -> Board
blindlyCastle c b s = update c King placedRook (<> squareAfter s c King)
  where placedRook = update c Rook removed (<> squareAfter s c Rook)
        removed = wipe b (squareBefore s c Rook <> squareBefore s c King)

-- * Helpers

-- | Is the board set up properly for a player to be able to castle?
canCastle :: Game -> Side -> Bool
canCastle g s = hasBlanks g s && hasOption g s && piecesAreRight g s

piecesAreRight :: Game -> Side -> Bool
piecesAreRight g s = (squareBefore s c Rook `submask` get c Rook b)
                        && (squareBefore s c King `submask` get c King b)
  where b = board g
        c = player g

-- | Are there blansk where there needs to be?
hasBlanks :: Game -> Side -> Bool
hasBlanks g s = between s (player g) `submask` blanks (board g)

-- | Does the game still have the option to castle?
hasOption :: Game -> Side -> Bool
hasOption g s = (player g, s) `elem` castleStatus g

-- | Are there the blanks between the king and rook required?
between :: Side -> Colour -> Mask
between Kingside  White = fromList [F1, G1]
between Kingside  Black = fromList [F8, G8]
between Queenside White = fromList [B1, C1, D1]
between Queenside Black = fromList [B8, C8, D8]

-- | The locations of pieces before they castle.
squareBefore :: Side -> Colour -> Piece -> Mask
squareBefore Queenside White Rook = mask $ coord A1
squareBefore Queenside Black Rook = mask $ coord A8
squareBefore Kingside  White Rook = mask $ coord H1
squareBefore Kingside  Black Rook = mask $ coord H8
squareBefore _ White King = mask $ coord E1
squareBefore _ Black King = mask $ coord E8
squareBefore _ _ _ = 0

-- | the locations of pieces after they castle
squareAfter :: Side -> Colour -> Piece -> Mask
squareAfter Kingside  White Rook = mask $ coord F1
squareAfter Kingside  White King = mask $ coord G1
squareAfter Queenside Black Rook = mask $ coord D8
squareAfter Queenside Black King = mask $ coord C8
squareAfter Kingside  Black Rook = mask $ coord F8
squareAfter Kingside  Black King = mask $ coord G8
squareAfter Queenside White Rook = mask $ coord D1
squareAfter Queenside White King = mask $ coord C1
squareAfter _ _ _ = mask $ coord OffBoard
