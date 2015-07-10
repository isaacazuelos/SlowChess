-- |
-- Module      : Game.SlowChess.Game
-- Description : A game
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model. This is the module responsible for building
-- 'Game' trees, and exporting only ways of manipulating them that remain
-- completely legal according the rules of chess.

module Game.SlowChess.Game ( -- * Constructors
                             Game
                           , start
                           , challange
                             -- * Accessors
                           , future
                           , lookAhead
                             -- * Game state information
                           , check
                           , checkmate
                           , draw
                             -- * Working with Games
                           , attacked
                           ) where

import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  (mempty, (<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Mask
import           Game.SlowChess.Move
import           Game.SlowChess.Move.Castle
import           Game.SlowChess.Move.EnPassant
import           Game.SlowChess.Move.Promotion
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Piece

-- * Constructors and accessors

-- | Start a new (normal) game of chess.
start :: Game
start = buildFutureBy legal g
  where g = Game { player       = White
                 , board        = starting
                 , ply          = Nothing
                 , past         = Nothing
                 , _future      = Nothing
                 , options      = allOptions
                 }

-- | Builds a game out of a challange board. This assumes that castling isn't
-- allowed, and that there were no moves prior to the existance of the board
-- as given.
challange :: Colour -> Board -> Game
challange c b = buildFutureBy legal g
  where g = Game { player       = c
                 , board        = b
                 , ply          = Nothing
                 , past         = Nothing
                 , _future      = Nothing
                 , options      = mempty
                 }

-- * Game-using rules.

-- Generate /all/ legal moves.
legal :: Rule
legal g = (moves g <> castle g <> castle g <> enPassant g) >>= promotions

future :: Game -> [Game]
future g = fromMaybe (legal g) (_future g)

-- | Get a bunch of future games a certain deapth ahead.
lookAhead :: Int -> Game -> [Game]
lookAhead n
    | n <= 0    = const []
    | n == 1    = future
    | otherwise = (>>= future) . lookAhead (n-1)

-- | Is the a game in check? A game is in check if the next player can move a
-- piece in a way that captures the king.
check :: Game -> Bool
check g = any (cappedKing (player g)) (future g)

-- | Is the game won? A game is in checkmate if the current player cannot
-- make a move which does not yeild check.
checkmate :: Game -> Bool
checkmate g = all (cappedKing (player g)) (future g)

-- | Has a game drawn? Games can draw due to a multitude of reasons.
-- see https://en.wikipedia.org/wiki/Draw_(chess)#Draws_in_all_games
draw :: Game -> Bool
draw = undefined

-- | Is there a king left for the player of a game.
cappedKing :: Colour -> Game -> Bool
cappedKing c = (== mempty) . get c King . board

-- * Working with games.

-- | Collects the attacked tiles into a mask.
attacked :: [Game] -> Mask
attacked = foldl (\ a g -> a <> maybe 0 mask (ply g >>= destination)) 0
