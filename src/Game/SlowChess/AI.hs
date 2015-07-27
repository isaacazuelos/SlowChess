-- |
-- Module      : Game.SlowChess.AI
-- Description : The chess AI interface
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This module ties together the game, scoring and tree searching together
-- to provide a way of suggesting

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.SlowChess.AI where

import           Game.SlowChess.AI.Internal
import           Game.SlowChess.AI.Negamax
import           Game.SlowChess.Game
import           Game.SlowChess.Evaluate

import           Data.List                  (sortBy)
import           Data.Ord                   (comparing)

-- | Chess games form a 'GameTree', in the obvious way.
instance GameTree Game where
    terminal g = null (future g) || checkmate g
    children = future
    evaluate = eval

-- | How many games into the future we look to pick the best game. Note that
-- since (partial) knoledge of the future game state is needed to know if
-- the game is in check, the actual depth searched is one larger.
maxSearchDepth :: Int
maxSearchDepth = 3

-- | Suggest one of the game's children
suggest :: Int -> Game -> Maybe Game
suggest n g = case suggestions n g of
                []  -> Nothing
                x:_ -> Just x

suggestions :: Int -> Game -> [Game]
suggestions n = sortBy (comparing $ nega n MaximizingPlayer) . children
