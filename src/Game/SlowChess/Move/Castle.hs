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

module Game.SlowChess.Move.Castle ( castle, blindlyDoCastle ) where

import           Control.Monad                (mzero)

import           Data.Monoid                  ((<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Mask          hiding (fromList)
import           Game.SlowChess.Piece

import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move.Internal

-- | Returns the castling moves for a game, even those that would put the
-- player in check. This checks the other rules. Note that castling only make
-- sense if the game started in the starting position, so this function
-- assumes that.
castle :: Game -> [Ply]
-- castle g = do let p = player g
--               s <- [Kingside, Queenside]
--               loc <- map (square s p) [King, Rook]
--               if (between s p `submask` blanks (board g))
--                 && (not . hasMoved loc) g && (not . hasCastled) g
--                 then return $ Castle p s
--                 else mzero
castle g = do s <- [Kingside, Queenside]
              if canCastle g s
                then return $ Castle (player g) s
                else mzero

-- | Do the actions to castle to a board, but /without/ any of the checks.
-- This just blindly removes the pieces, and puts a king and rook where
-- they ought to land.
blindlyDoCastle :: Colour -> Board -> Side -> Board
blindlyDoCastle c b s = update c King placedRook (<> mask (square s c King))
  where placedRook = update c Rook removed (<> mask (square s c Rook))
        removed = wipe b (mask (square s c Rook) <> mask (square s c King))

-- | Is the board set up properly for a player to be able to castle?
-- There's still the issue of weather or not the pieces have moved, so this
-- isn't all of the relevant rules.
canCastle :: Game -> Side -> Bool
canCastle g s = (square s c Rook `on` get c Rook b)
                    && (square s c King `on` get c King b)
                    && (between s c `submask` blanks b)
                    && not (hasMoved g (square s c King)
                            || hasMoved g (square s c Rook))
                    && not (hasCastled g)
  where b = board g
        c = player g

-- | The locations of pieces when they castle.
square :: Side -> Colour -> Piece -> Coord
square Queenside White Rook = coord A1
square Queenside Black Rook = coord A8
square Kingside  White Rook = coord H1
square Kingside  Black Rook = coord H8
square _ White King = coord E1
square _ Black King = coord E8
square _ _ _ = coord OffBoard

-- | Are there the blanks between the king and rook required?
between :: Side -> Colour -> Mask
between Kingside  White = fromList [F1, G1]
between Kingside  Black = fromList [F8, G8]
between Queenside White = fromList [B1, C1, D1]
between Queenside Black = fromList [B8, C8, D8]

-- | Has the the piece at the coordinate moved during the game? Since this
-- module assumes castling in a regular chess game, it checks if the pieces
-- have moved.
hasMoved :: Game -> Coord -> Bool
hasMoved g c = any (moved . snd) . history $ g
  where moved (Move _ _ s _) = c == s
        -- Other kinds of moves can't happen to castling pieces, so we only
        -- need to check this one.
        moved _ = False

-- | Has the current player already castled this game?
hasCastled :: Game -> Bool
hasCastled g = any (isCastle (player g) . snd) (history g)
  where isCastle c (Castle p _) = c == p
        isCastle _ _ = False
