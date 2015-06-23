-- |
-- Module      : Game.SlowChess.Pretty
-- Description : Pretty printing of game-related data.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Pretty is a tool for pretty printing the chess data types.

module Game.SlowChess.Pretty (Pretty (pretty), pprint) where

import           Data.List            (intersperse, sort)

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
import           Game.SlowChess.Piece
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move

-- | A pretty pretty printing typeclass like @Show@, but with more of an
-- emphasis on being human readable. Useful for the REPL.
class Pretty a where
  pretty :: a -> String

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
instance Pretty Mask where
  pretty = buildBoard . fromMask "#"

-- | Boards are prettied like masks, but with the Unicode characters for the
-- chess pieces to show which piece occupies a square.
instance Pretty Board where
  pretty b = if conflicts b /= 0
               then "Invalid Board."
               else buildBoard (concat
                                [ fromMask "R" $ get Black Rook   b
                                , fromMask "H" $ get Black Knight b
                                , fromMask "B" $ get Black Bishop b
                                , fromMask "Q" $ get Black Queen  b
                                , fromMask "K" $ get Black King   b
                                , fromMask "P" $ get Black Pawn   b
                                , fromMask "r" $ get White Rook   b
                                , fromMask "h" $ get White Knight b
                                , fromMask "b" $ get White Bishop b
                                , fromMask "q" $ get White Queen  b
                                , fromMask "k" $ get White King   b
                                , fromMask "p" $ get White Pawn   b
                                ])

-- | Pretty print a game
instance Pretty Game where
  pretty (Game p b co ca h) = pretty b ++
                          "\n  player: " ++ show p ++
                          "\n  condition: " ++ show co ++
                          "\n  caslting: " ++ pretty ca ++
                          "\n  moves: " ++ (show . length) h

-- | Castling state is pretty printed in just a nicer english representation.
instance Pretty Castle where
  pretty (Castle True True)   = "both"
  pretty (Castle True False)  = "only white"
  pretty (Castle False True)  = "only black"
  pretty (Castle False False) = "neither"

-- | This should eventually look something more like "a1->b2".
instance Pretty Ply where
  pretty (Move s t) = buildBoard $ (fromMask "s" s) ++ (fromMask "t" t)

instance Pretty a => Pretty [a] where
  pretty xs = concat $ intersperse "\n" (map pretty xs)

-- | Like 'print' but it uses 'Pretty' rather than 'Show'.
pprint :: Pretty a => a -> IO ()
pprint = putStrLn . pretty

-- | A tile is a board index followed by the string we'll use to represent
-- what is occupying that index.
type Tile = (Int, String)

-- | Builds a partial list of tiles from the occupied squares on a board.
fromMask :: String -> Mask -> [Tile]
fromMask s = map (\i -> (i,s)) . toList

-- | Adds blank tiles to a list of tiles. The length of the resulting list is
-- always exactly 64 tiles.
fill :: [Tile] -> [Tile]
fill ts = sort $ take 64 $ ts ++ map (\i -> (i, "_")) unused
  where used   = map fst ts
        unused = filter (`notElem` used) [0..63]

-- | Builds a board from a list of the tiles.
buildBoard :: [Tile] -> String
buildBoard ts = unlines (rows ++ columnLegend)
  where columnLegend = ["  a b c d e f g h"]
        ranks = (map concat . chunks 8 . map snd . fill) ts
        rows  = zipWith buildRank [(8 :: Int),7..] ranks
        buildRank r ts' = show r ++ "|" ++ intersperse '|' ts' ++ "|"

-- | When the integer argument is @n@, it splits a list into pieces of length
-- @n@. The last piece will be shorter if @n@ does not evenly divide the
-- length of the list. When @n == 0@, it creates an infinite list of empty
-- lists.
chunks :: Int -> [a] -> [[a]]
chunks n xs = chunks' n xs []
  where chunks' _ [] acc = acc
        chunks' i ys acc = chunks' i (drop i ys) (take i ys:acc)
