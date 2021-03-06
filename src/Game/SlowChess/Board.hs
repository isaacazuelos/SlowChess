-- |
-- Module      : Game.SlowChess.Board
-- Description : Board representation.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess board is just the tiles of the 8x8 board, and which kind of piece
-- they're occupied by.

module Game.SlowChess.Board ( -- * Board Creation
                              Board
                            , blank
                            , starting
                              -- * Modification
                            , get
                            , set
                            , update
                            , setMany
                            , wipe
                              -- * Operations
                            , material
                            , blanks
                            , nonFriendly
                            ) where

import           Data.Bits            (xor, complement)

import           Game.SlowChess.Coord
import           Game.SlowChess.Mask  hiding (fromList)
import           Game.SlowChess.Piece

-- * Board

-- | A chess board is represented as the state of the pieces of both sides.
-- This is done by keeping track of all of the places where there are pieces
-- of a colour, and also where there are pieces of a piece type.
data Board = Board { pawns   :: !Mask
                   , rooks   :: !Mask
                   , knights :: !Mask
                   , bishops :: !Mask
                   , kings   :: !Mask
                   , queens  :: !Mask
                   , whites  :: !Mask
                   , blacks  :: !Mask
                   } deriving ( Eq )

-- | Boards are prettied like masks, but with the Unicode characters for the
-- chess pieces to show which piece occupies a square.
instance Show Board where
 show b = if conflicts b /= 0
              then "Invalid Board."
              else buildBoardString (concat
                    [ boardStringTiles "♜" $ get Black Rook   b
                    , boardStringTiles "♞" $ get Black Knight b
                    , boardStringTiles "♝" $ get Black Bishop b
                    , boardStringTiles "♛" $ get Black Queen  b
                    , boardStringTiles "♚" $ get Black King   b
                    , boardStringTiles "♟" $ get Black Pawn   b
                    , boardStringTiles "♖" $ get White Rook   b
                    , boardStringTiles "♘" $ get White Knight b
                    , boardStringTiles "♗" $ get White Bishop b
                    , boardStringTiles "♕" $ get White Queen  b
                    , boardStringTiles "♔" $ get White King   b
                    , boardStringTiles "♙" $ get White Pawn   b
                    ])

-- | All of the positions held by pieces of a colour.
material :: Colour -> Board -> Mask
{-# INLINE material #-}
material White (Board _ _ _ _ _ _ w _) = w
material Black (Board _ _ _ _ _ _ _ b) = b

-- | All of the empty squares on the board.
blanks :: Board -> Mask
blanks b = invert (squish (material White b) (material Black b))

-- | All of the squares not held by pieces of a colour --- so either
-- blanks or enemy pieces.
{-# INLINE nonFriendly #-}
nonFriendly :: Colour -> Board -> Mask
nonFriendly White (Board _ _ _ _ _ _ (Mask w) _) = Mask (complement w)
nonFriendly Black (Board _ _ _ _ _ _ _ (Mask b)) = Mask (complement b)

-- | A blank board is the board with no pieces on it.
{-# INLINE blank #-}
blank :: Board
blank = Board 0 0 0 0 0 0 0 0

-- | A board with the pieces in their starting positions.
starting :: Board
starting = Board { rooks   = fromList  [A1, H1, A8, H8]
                 , knights = fromList  [B1, G1, B8, G8]
                 , bishops = fromList  [C1, F1, C8, F8]
                 , kings   = fromList  [E1, E8]
                 , queens  = fromList  [D1, D8]
                 , pawns   = fromList ([A2 .. H2] ++ [A7 .. H7])
                 , whites  = fromList  [A1 .. H2]
                 , blacks  = fromList  [A7 .. H8]
                 }

-- | Gets a mask of the locations of a particular type of piece of one colour.
-- The following property holds:
--
-- > get c p (set c p b m) == m
get :: Colour -> Piece -> Board -> Mask
get c Rook   b = both (material c b) (rooks   b)
get c Knight b = both (material c b) (knights b)
get c Bishop b = both (material c b) (bishops b)
get c Queen  b = both (material c b) (queens  b)
get c King   b = both (material c b) (kings   b)
get c Pawn   b = both (material c b) (pawns   b)

-- | Modify the positions of a type of piece (of a colour) on a board. Unlike
-- '@set@' this does not attempt to capture other types of pieces, and can
-- lead to invalid board states if incorrectly used — that's why it's not
-- exposed.
modify :: Colour -> Piece -> Board -> Mask -> Board
modify c p b m = case c of
    White -> (updatePieces m c p b) { whites = newMaterial }
    Black -> (updatePieces m c p b) { blacks = newMaterial }
  where newMaterial              = squish m (material c b `minus` get c p b)
        newPieces    m' c' p' b' = squish m' (get (enemy c') p' b')
        updatePieces m' c' p' b' = case p of
            Rook   -> b' { rooks   = newPieces m' c' p' b' }
            Knight -> b' { knights = newPieces m' c' p' b' }
            Bishop -> b' { bishops = newPieces m' c' p' b' }
            Queen  -> b' { queens  = newPieces m' c' p' b' }
            King   -> b' { kings   = newPieces m' c' p' b' }
            Pawn   -> b' { pawns   = newPieces m' c' p' b' }

-- | Apply a mask transformation to each mask in mask in the board — even the
-- material masks.
{-# INLINE forEach #-}
forEach :: Board -> (Mask -> Mask) -> Board
forEach (Board a b c d e f g h) f' = Board (f' a) (f' b) (f' c) (f' d)
                                           (f' e) (f' f) (f' g) (f' h)


-- | All the positions on a board which appear on more than one mask. The
-- following property holds:
--
-- > conflicts (set c p b m) == conflicts b
conflicts :: Board -> Mask
conflicts (Board a b c d e f g h) = foldr xor' 0 [a, b, c, d, e, f, g, h]
  where xor' (Mask x) (Mask y) = Mask (xor x y)

-- | Set the positions of a type of piece on a board, ensuring that no other
-- type of piece is at that position by overwriting them.
{-# INLINE set #-}
set :: Colour -> Piece -> Board -> Mask -> Board
set c p b m = modify c p (wipe b m) m

-- | Wipe the pieces off some squares.
{-# INLINE wipe #-}
wipe :: Board -> Mask -> Board
wipe b m = forEach b (`minus` m)

-- | Update the positions of a type of piece on a board using a mask of pieces
-- to add and wipe.
update :: Colour -> Piece -> Board -> Mask -> Mask -> Board
update c p b a = wipe (set c p b (squish a (get c p b)))

-- | Set a bunch of pieces up at once. This function is designed to make it
-- easy to write Haskell to set up custom boards. Like 'set' it prevents
-- conflicts -- later elements in the list override earlier ones.
setMany :: Board -> [(Colour, Piece, [CoordName])] -> Board
setMany = foldl (\ b' (c, p, ns) -> set c p b' (fromList ns))
