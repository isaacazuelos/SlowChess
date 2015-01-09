-- |
-- Module      : Game.SlowChess.Board
-- Description : Board representation.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess board is just the tiles of the board 8x8 board, and which kind of
-- piece they're occupied by.

module Game.SlowChess.Board {-
                            ( Board
                            , blank
                            , starting
                            , get
                            , set
                            , material
                            )
                            -} where

import           Data.Bits            (complement, xor)
import           Data.Monoid          ((<>))

import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

-- * Board

-- | A chess board is represented as the state of the pieces of both sides.
-- This is done by keeping track of all of the places where there are pieces
-- of a colour, and also where there are pieces of a piece type.
data Board = Board { pawns   :: Mask
                   , rooks   :: Mask
                   , knights :: Mask
                   , bishops :: Mask
                   , kings   :: Mask
                   , queens  :: Mask
                   , whites  :: Mask
                   , blacks  :: Mask
                   } deriving (Show, Eq)

-- | All of the positions held by pieces of a colour.
material :: Colour -> Board -> Mask
material White = whites
material Black = blacks

-- | All of the empty squares on the board.
blanks :: Board -> Mask
blanks b = complement ((material White b) <> (material Black b))

-- | A blank board is the board with no pieces on it.
blank :: Board
blank = Board 0 0 0 0 0 0 0 0

-- | A board with the pieces in their starting positions.
starting :: Board
starting = Board { rooks   = fromList [0,7,56,63]
                 , knights = fromList [1,6,57,62]
                 , bishops = fromList [2,5,58,61]
                 , kings   = fromList [4,59]
                 , queens  = fromList [3,60]
                 , pawns   = fromList ([48..55] ++ [8..15])
                 , whites  = fromList [0..15]
                 , blacks  = fromList [48..63]
                 }

-- | Gets a mask of the locations of a particular type of piece of one colour.
-- The following property holds:
--
-- > get c p (set c p b m) == m
get :: Colour -> Piece -> Board -> Mask
get c Rook   b = both (material c b) (rooks   b)
get c Knight b = both (material c b) (knights b)
get c Bishop b = both (material c b) (bishops b)
get c Queen  b = both (material c b) (kings   b)
get c King   b = both (material c b) (queens  b)
get c Pawn   b = both (material c b) (pawns   b)

-- | Update the positions of a type of piece (of a colour) on a board. Unlike
-- '@set@' this does not attempt to capture other types of pieces, and can
-- lead to invalid board states if incorrectly used — that's why it's not
-- exposed.
update :: Colour -> Piece -> Board -> Mask -> Board
update c p b m = case c of
    White -> (updatePieces m c p b) { whites = newMaterial }
    Black -> (updatePieces m c p b) { blacks = newMaterial }
  where newMaterial =  m <> (material c b `minus` get c p b)
        newPieces    m' c' p' b' = m' <> get (enemy c') p' b'
        updatePieces m' c' p' b' = case p of
            Rook   -> b' { rooks   = newPieces m' c' p' b' }
            Knight -> b' { knights = newPieces m' c' p' b' }
            Bishop -> b' { bishops = newPieces m' c' p' b' }
            Queen  -> b' { kings   = newPieces m' c' p' b' }
            King   -> b' { queens  = newPieces m' c' p' b' }
            Pawn   -> b' { pawns   = newPieces m' c' p' b' }

-- | Apply a mask transformation to each mask in mask in the board — even the
-- material masks.
forEach :: Board -> (Mask -> Mask) -> Board
forEach (Board a b c d e f g h) f' = Board (f' a) (f' b) (f' c) (f' d)
                                           (f' e) (f' f) (f' g) (f' h)

-- | All the positions on a board which appear on more than one mask. The
-- following property holds:
--
-- > conflicts (set c p b m) == mempty
conflicts :: Board -> Mask
conflicts (Board a b c d e f g h) = foldr xor 0 [a, b, c, d, e, f, g, h]

-- | Set the positions of a type of piece on a board, ensuring that no other
-- type of piece is at that position.
set :: Colour -> Piece -> Board -> Mask -> Board
set c p b m = update c p (forEach b (`minus` m)) m
