{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Game.SlowChess.Mask
-- Description : Piece layout masks and movements.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Masks on a chess board indication the inclusion of exclusion of each square
-- on a board. Masks can move these squares around in certain directions, and
-- handle reasonable queries about the board.

module Game.SlowChess.Mask ( -- * Constructing Masks
                             Mask (Mask)
                           , fromList
                           , toList
                             -- * Mask Operations
                           , both
                           , minus
                           , invert
                           , submask
                           , count
                             -- * Movement
                           , hop
                             -- * Printing tools
                           , BoardStringTile
                           , buildBoardString
                           , boardStringTiles
                           ) where

import           Data.Bits
import           Data.List            (intersperse, sort, (\\))
import           Data.Monoid
import           Data.Word

import           Game.SlowChess.Piece

-- A Mask uses the 64 bits of a machine word to indicate which of the 64
-- squares on a chess board are being used.

-- | This is used both to track the squares being reasoned about, and also to
-- track the positions of types of pieces in a game. The squares are numbered
-- according to the board below.
--
-- >    8 56 57 58 59 60 61 62 63
-- >    7 48 49 50 51 52 53 54 55
-- >    6 40 41 42 43 44 45 46 47
-- >    5 32 33 34 35 36 37 38 39
-- >    4 24 25 26 27 28 29 30 31
-- >    3 16 17 18 19 20 21 22 23
-- >    2 08 09 10 11 12 13 14 15
-- >    1 00 01 02 03 04 05 06 07
-- >      a  b  c  d  e  f  g  h
newtype Mask = Mask Word64 deriving ( Eq, Bits, Num, Ord )

-- | Masks form a monoid where the identity is an empty mask and our operation
-- preserves the occupied spaces of both operands. Note that this monoid is
-- commutative (and associative.)
--
-- > 1 0 0    1 0 0   1 0 0
-- > 0 1 0 <> 1 0 0 = 1 1 0
-- > 0 0 1    1 0 0   1 0 1
instance Monoid Mask where
  mempty  = 0
  mappend = (.|.)

-- | Masks are prettied using the normal grid, with @#@ indicating an occupied
-- square.
--
-- Here's an example mask with squares along the diagonal occupied.
--
-- > 8|#|_|_|_|_|_|_|_|
-- > 7|_|#|_|_|_|_|_|_|
-- > 6|_|_|#|_|_|_|_|_|
-- > 5|_|_|_|#|_|_|_|_|
-- > 4|_|_|_|_|#|_|_|_|
-- > 3|_|_|_|_|_|#|_|_|
-- > 2|_|_|_|_|_|_|#|_|
-- > 1|_|_|_|_|_|_|_|#|
-- >   a b c d e f g h
instance Show Mask where
  show = buildBoardString . boardStringTiles "#"

-- | Create a mask which has only a single index set.
--
-- >                   1 0 0
-- > maskFromIndex 0 = 0 0 0
-- >                   0 0 0
maskFromIndex :: Int -> Mask
maskFromIndex i = if (i >= 0) && (i < 64)
                  then Mask (2^i)
                  else error "Index out of range for chess board."

-- | Build a mask from a list of square indexes.
--
-- >                    1 0 0
-- > fromList [0,4,8] = 0 1 0
-- >                    0 0 1
fromList :: [Int] -> Mask
fromList = foldr ((<>) . maskFromIndex) mempty

-- | Build a list of square indicies from a mask.
--
-- @fromList . toList = toList . fromList = id@
toList :: Mask -> [Int]
toList m = filter (\i -> mempty /= m .&. maskFromIndex i) [0..63]

-- | All the squares which are occupied on exactly both of the inputs.
both :: Mask -> Mask -> Mask
both = (.&.)

-- | Unmark all the position in the first mask that are held by the second.
--
-- > 1 0 1         1 1 0   0 0 1
-- > 0 1 0 `minus` 1 1 0 = 0 0 0
-- > 1 0 1         0 0 0   1 0 1
minus :: Mask -> Mask -> Mask
minus a b = a .&. complement b

-- | Inverts which positions are marked and which are not.
--
-- >        1 0 1   0 0 1
-- > invert 0 1 0 = 0 0 0
-- >        1 0 1   1 0 1
invert :: Mask -> Mask
invert = complement

-- | Is one mask contained in the other?
--
-- > 0 0 0           1 0 1
-- > 0 1 0 `submask` 0 1 0 = True
-- > 0 0 0           1 0 1
submask :: Mask -> Mask -> Bool
submask a b = b == a <> b

-- | Count the number of pieces on the mask.
count :: Mask -> Int
count = popCount

-- ** Movement

-- The basic motions are done through shifts. Since these shifts *could* cause
-- certain positions to wrap around, we need to bitwise 'and' the board to be
-- moved with a mask that indicates which positions *can* be moved in that
-- direction.

-- | The pieces indicated by a mask can only move in a direction if doing so
-- would not cause the piece to move off the board. @'canMove dir mask'@
-- removes any positions in @mask@ which could not move in the direction
-- @dir@.
moveable :: Direction -> Mask -> Mask
moveable N  = (.&.) $ fromList [0..55]
moveable NE = moveable N . moveable E
moveable E  = (.&.) $ fromList ([0..63] \\ [7,15,23,31,39,47,55,63])
moveable SE = moveable S . moveable E
moveable S  = (.&.) $ fromList [8..63]
moveable SW = moveable S . moveable E
moveable W  = (.&.) $ fromList ([0..63] \\ [0,8,16,24,32,40,48,56])
moveable NW = moveable N . moveable W

-- |  Hop moves the pieces of a mask in the specified direction.
--
-- The positions indicated by a mask can move up or down their ranks or
-- files — more complicated movement can be constructed as the composition of
-- these basic motions.
--
-- The pieces indicated by a mask can only move in a direction if doing so
-- would not cause the piece to move off the board, so it holds that:
--
-- > hop d m == hop d (moveable d m)
hop :: Direction -> Mask -> Mask
hop N  = (`shiftL` 8) . moveable N
hop NE = hop N . hop E
hop E  = (`shiftL` 1) . moveable E
hop SE = hop S . hop E
hop S  = (`shiftR` 8) . moveable S
hop SW = hop S . hop W
hop W  = (`shiftR` 1) . moveable W
hop NW = hop N . hop W

-- * Printing Masks

-- | A BoardStringTile is a board index followed by the string we'll use to represent
-- what is occupying that index.
type BoardStringTile = (Int, String)

-- | Builds a partial list of BoardStringTiles from the occupied squares on a board.
boardStringTiles :: String -> Mask -> [BoardStringTile]
boardStringTiles s = map (flip (,) s) . toList

-- | Adds blank BoardStringTiles to a list of BoardStringTiles. The length of the resulting list is
-- always exactly 64 BoardStringTiles.
fill :: [BoardStringTile] -> [BoardStringTile]
fill ts = sort $ take 64 $ ts ++ map (\i -> (i, "_")) unused
  where used   = map fst ts
        unused = filter (`notElem` used) [0..63]

-- | Builds a board from a list of the BoardStringTiles.
buildBoardString :: [BoardStringTile] -> String
buildBoardString ts = unlines (rows ++ columnLegend)
  where columnLegend = ["  A B C D E F G H"]
        ranks = (map concat . chunks 8 . map snd . fill) ts
        rows  = "\n" : zipWith buildRank [(8 :: Int),7..] ranks
        buildRank r ts' = show r ++ "|" ++ intersperse '|' ts' ++ "|"

-- | When the integer argument is @n@, it splits a list into pieces of length
-- @n@. The last piece will be shorter if @n@ does not evenly divide the
-- length of the list. When @n == 0@, it creates an infinite list of empty
-- lists.
chunks :: Int -> [a] -> [[a]]
chunks n xs = chunks' n xs []
  where chunks' _ [] acc = acc
        chunks' i ys acc = chunks' i (drop i ys) (take i ys:acc)
