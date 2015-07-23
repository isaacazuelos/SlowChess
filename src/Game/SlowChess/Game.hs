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
                           , enableCastling
                             -- * Gernerating Rules
                           , moves
                           , castle
                           , enPassant
                           , promotions
                             -- * Restrictive Rules
                           , fifty
                           , threeFold
                             -- * Accessors
                           , player
                           , future
                           , board
                           , ply
                           , lookAhead
                             -- * Game state predicates
                           , check
                           , checkmate
                           , draw
                             -- * Working with Games
                           , attacked
                           ) where

import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   (mempty, (<>))

import           Game.SlowChess.Board
import           Game.SlowChess.Coord
import           Game.SlowChess.Game.Fifty
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Game.ThreeFold
import           Game.SlowChess.Mask
import           Game.SlowChess.Move
import           Game.SlowChess.Move.Castle
import           Game.SlowChess.Move.EnPassant
import           Game.SlowChess.Move.Internal
import           Game.SlowChess.Move.Promotion
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
                 , castleStatus = allOptions
                 , drawStatus   = Normal
                 , fiftyStatus  = 0
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
                 , castleStatus = mempty
                 , drawStatus   = Normal
                 , fiftyStatus  = 0
                 }

-- | Re-enable all castling options for a 'Game', typically for setting up
-- a 'challange' game.
enableCastling :: Game -> Game
enableCastling g = g { castleStatus = allOptions }

-- * Game-using rules.

-- | Generate the moves which are legal, were it not for the rule about not
-- moving into check.
legal :: Rule
legal g = (moves g <> castle g <> enPassant g)
            >>= promotions
            >>= fifty
            >>= threeFold

-- | All legal futures of a game.
future :: Game -> [Game]
future g = fromMaybe (filter (not . check) (legal g)) (_future g)

-- | Get a bunch of future games a certain deapth ahead.
lookAhead :: Int -> Game -> [Game]
lookAhead n
    | n <= 0    = const []
    | n == 1    = future
    | otherwise = (>>= future) . lookAhead (n-1)

-- | Is the a game in check? A game is in check if the current player's king
-- could be captured if they do nothing.
check :: Game -> Bool
check g = kingIs (/= 0) (togglePlayer g) && any (kingIs (== 0)) (future g)
  where kingIs f g' = f $ get (player g') King (board g')

-- | Is the game won? A game is in checkmate if the current player's king will
-- remain in check regardless of how they play.
checkmate :: Game -> Bool
checkmate g = check g && hasKing && kings `submask` attacked (future g)
  where kings = kingTargets (player g) (board g)
        hasKing = 0 /= get (player g) King (board g)

-- | Has a game drawn? Games can draw due to a multitude of reasons.
-- see https://en.wikipedia.org/wiki/Draw_(chess)#Draws_in_all_games
draw :: Game -> Bool
draw g = drawStatus g /= Normal

-- * Working with games.

-- | Collects the attacked tiles into a mask.
attacked :: [Game] -> Mask
attacked = foldl (\ a g -> a <> maybe 0 mask (ply g >>= destination)) 0
