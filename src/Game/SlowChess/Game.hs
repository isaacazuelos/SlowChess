-- |
-- Module      : Game.SlowChess.Game
-- Description : A game
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model. This is the module responsible for building
-- 'Game' trees, and exporting only ways of manipulating them that remain
-- completely legal according the rules of chess.

module Game.SlowChess.Game ( -- * Constructors and accessors
                             Game
                           , start
                           , challange
                             -- * Generating games
                           , legal
                           , apply
                             -- * Game state information
                           , check
                           , checkmate
                           , draw
                           ) where

import           Data.Monoid                  (mempty)

import           Game.SlowChess.Board
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Move
import           Game.SlowChess.Move.Castle
import           Game.SlowChess.Piece

-- * Constructors and accessors

-- | Start a new (normal) game of chess.
start ::  Game
start = g
  where g = Game { player       = White
                 , board        = starting
                 , ply          = Nothing
                 , past         = Nothing
                 , future       = legal g
                 , options      = allOptions
                 }

-- | Builds a game out of a challange board. This assumes that castling isn't
-- allowed, and that there were no moves prior to the existance of the board
-- as given.
challange :: Colour -> Board -> Game
challange c b = g
  where g = Game { player       = c
                 , board        = b
                 , ply          = Nothing
                 , past         = Nothing
                 , future       = legal g
                 , options      = mempty
                 }

-- * Generating games

-- | All the legal games which can follow from a game.
legal :: Game -> [Game]
legal = undefined

-- | Apply a ply to a game, giving back a game if the ply doesn't lead to
-- something crazy.
apply :: Game -> Ply -> Maybe Game
apply = undefined 

-- * Game-using rules.

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
