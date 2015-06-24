-- |
-- Module      : Game.SlowChess.Game.Internal
-- Description : A game state in an unsafe way.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A chess game model presented with the implementation details
-- exposed in such a way as to allow for the creating and manipulation
-- of otherwise illegal states.
--
-- This module exists for two reasons.
--
-- 1. It allows for developers to get their hands dirty with the
--    implementation details if they wish, allowing for the creation
--    of replacements of 'Game.SlowChess.Game' that might extend or
--    modify the rules.
--
-- 2. It allows us to avoid the cyclical dependency issues that arise
--    from the existence of the /special movements/ as described in
--    'Game.SlowChess.Movement' --- where moves require game
--    knowledge, but to preserve the consistency of the game, valid
--    movement knowledge is required.

module Game.SlowChess.Game.Internal where

import           Game.SlowChess.Board
import           Game.SlowChess.Piece

-- | A raw game state. Since this is still being sketched, this is
-- likely to change a lot over the next few commits.
data Game = Game { player    :: Colour
                 , board     :: Board
                 , condition :: Condition
                 , castle    :: Castle
                 , history   :: [Game]
                 } deriving (Show, Eq)

-- | Information about weather castling is allowed for each player.
data Castle = Castle Bool Bool deriving (Show, Eq)

-- | Can white castle?
whiteCastle :: Castle -> Bool
whiteCastle (Castle w _) = w

-- | Can black castle?
blackCastle :: Castle -> Bool
blackCastle (Castle _ b) = b

-- | The conditions a game can be in, where 'Normal' is just the
-- normal play state, or the absence of any sort of check state.
data Condition = Normal | Check | Checkmate deriving (Show, Eq)

