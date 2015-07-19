-- |
-- Module      : Game.SlowChess.AI
-- Description : The chess AI interface
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This module ties together the game, scoreing and tree searching together
-- to provide a way of suggesting 

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.SlowChess.AI where

import           Game.SlowChess.Game
import           Game.SlowChess.AI.MinMax
import           Game.SlowChess.AI.Internal

import           Data.List (sortBy)
import           Data.Ord  (comparing)

-- | Chess games form a 'GameTree', in the obvious way.
instance GameTree Game where
    terminal g = null (future g) || checkmate g
    children = future
    score = const 0

-- | How many games into the future we look to pick the best game. Note that
-- since (partial) knoledge of the future game state is needed to know if
-- the game is in check, the actual depth searched is one larger.
maxSearchDepth :: Int
maxSearchDepth = 3

-- | Suggest one of the game's children
suggest :: Game -> [Game]
suggest = sortBy (comparing $ minmax maxSearchDepth) . children
