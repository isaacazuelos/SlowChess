-- |
-- Module      : Game.SlowChess.Castle
-- Description : Castling related functions
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- When the king and the a rook have not moved, they can castle --- moving the
-- king two squares towards a rook on the player's first rank, then moving the
-- rook to the square over which the king crossed.
--
-- Castling may only be done if:
--
-- 1. The king has never moved, the rook involved has never moved.
--
-- 2. The squares between the king and the rook involved are unoccupied
--
-- 3. The king is not in check, and the king does not cross over or end on a
--    square in which it would be in check.
--
-- This module only checks teh first two rules and assumes that the board
-- started in the typical starting position.

module Game.SlowChess.Move.Castle (castle) where

import           Control.Monad                (guard)

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Mask          hiding (fromList)
import           Game.SlowChess.Piece

import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move.Internal

-- | Returns the castling moves for a game,  even those that would put the
-- player in check. This checks the other rules. Note that castling only make
-- sense if the game started in the starting position, so this function
-- assumes that.
castle :: Game -> [Ply]
castle g = do let p = player g
              s <- [Kingside, Queenside]
              guard $ between s p `submask` blanks (board g)
              loc <- map (square s p) [King, Rook]
              guard $ (not . hasMoved loc) g
              guard $ (not . hasCastled) g
              return $ Castle p s

-- | The locations of pieces when they castle.
square :: Side -> Colour -> Piece -> Coord
square Queenside White Rook = coord A1
square Queenside Black Rook = coord H8
square Kingside  White Rook = coord H1
square Kingside  Black Rook = coord A8
square _ White King = coord E1
square _ Black King = coord E8
square _ _ _ = coord OffBoard

-- | Are there the blanks between the king and rook required?
between :: Side -> Colour -> Mask
between Queenside White = fromList [B1, C1, D1]
between Queenside Black = fromList [B8, C8, D8]
between Kingside  White = fromList [F1, G1]
between Kingside  Black = fromList [F8, G8]

-- | Has the the piece at the coordinate moved during the game? Since this
-- module assumes castling in a regular chess game, it checks if the pieces
-- have moved.
hasMoved :: Coord -> Game -> Bool
hasMoved c = any (moved . snd) . history
  where moved (Move _ _ s _) = c == s
        -- Other kinds of moves can't happen to the staring pieces, so we only
        -- need to check this one.
        moved _ = False

-- | Has the current player already castled this game?
hasCastled :: Game -> Bool
hasCastled g = any (isCastle (player g) . snd) (history g)
  where isCastle c (Castle p _) = c == p
        isCastle _ _ = False
