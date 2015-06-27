-- |
-- Module      : Game.SlowChess.Move.Coord
-- Description : Coordinate systems.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A way of referring to a single tile on a board. Unlike 'Mask' a 'Coord' is
-- kept to a single square, or 'None' for no square at all.

module Game.SlowChess.Coord ( Coord
                            , CoordName ( A1, B1, C1, D1, E1, F1, G1, H1
                                        , A2, B2, C2, D2, E2, F2, G2, H2
                                        , A3, B3, C3, D3, E3, F3, G3, H3
                                        , A4, B4, C4, D4, E4, F4, G4, H4
                                        , A5, B5, C5, D5, E5, F5, G5, H5
                                        , A6, B6, C6, D6, E6, F6, G6, H6
                                        , A7, B7, C7, D7, E7, F7, G7, H7
                                        , A8, B8, C8, D8, E8, F8, G8, H8
                                        , OffBoard
                                        )
                            , coord
                            , name
                            , mask
                            , nowhere
                            , fromList
                            , merge
                            , split
                            , on
                            , hop
                            ) where

import Data.Monoid ((<>))

import Game.SlowChess.Piece

import qualified Game.SlowChess.Mask as M

-- | Just like a 'Mask' that contains at most one piece marked. Coords can
-- also be /off board/ in certain circumstances --- say after a 'hop'.
newtype Coord = Coord M.Mask deriving ( Show, Eq )

-- | The coordinates are ordered, as given by the diagrams in 'Mask'.
instance Enum Coord where
    toEnum   n = Coord (M.Mask (2^n))
    fromEnum (Coord m) = if null lst then fromEnum OffBoard else head lst
      where lst = M.toList m

-- | A nicer, human-readable way of naming the tiles of a chess board.
data CoordName = A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1
               | A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2
               | A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3
               | A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4
               | A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5
               | A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6
               | A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7
               | A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8
               | OffBoard
               deriving (Show, Eq, Enum)

-- | Create a coord from a nicer, human-readable name.
coord :: CoordName -> Coord
coord = toEnum . fromEnum

-- | Get the nicer, human-readable form of a 'Coord'.
name :: Coord -> CoordName
name = toEnum . fromEnum

-- | Get a 'Mask' version of a 'Coord'.
mask :: Coord -> M.Mask
mask (Coord m) = m

-- | The location that represents locations off of the board.
-- They're all equal.
nowhere :: Coord
nowhere = coord OffBoard

-- | Create a 'Mask' from a list of human-readable coordinate names.
fromList :: [CoordName] -> M.Mask
fromList = M.fromList . map fromEnum

-- | Merges two 'Coord's into a 'Mask'. Since 'Coord' has at most one piece
-- marked it can't return another 'Coord', and thus isn't a monoid like
-- 'Mask'.
merge :: Coord -> M.Mask -> M.Mask
merge (Coord a) b = a <> b

-- | Split a 'Mask' into a list of of the occupied 'Coord's.
split :: M.Mask -> [Coord]
split m = map (toEnum . fromEnum) (M.toList m)

-- | Are the tiles of the 'Coord' marked on the 'Mask'?
-- Returns 'False' if the Coord is None since that already means the piece
-- is off the board.
on :: Coord -> M.Mask -> Bool
on (Coord c) m = M.both c m == m

-- |  Hop moves the piece in the specified direction.
--
-- The piece can only move in a direction if doing so would not cause the
-- piece to move off the board. Coords that would hopp off the board turn into
-- 'None'
hop :: Direction -> Coord -> Coord
hop d (Coord m) = Coord (M.hop d m)
