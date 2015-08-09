-- |
-- Module      : Game.SlowChess.AI
-- Description : The chess AI interface
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This module ties together the game, scoring and tree searching together
-- to provide a way of suggesting

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.SlowChess.AI ( Score, suggest ) where

import           Game.SlowChess.AI.Internal
import           Game.SlowChess.Evaluate
import           Game.SlowChess.Game
import           Game.SlowChess.Game.Internal

import           Data.List                    (sortBy)
import           Data.Ord                     (comparing)

-- | Chess games form a 'GameTree', in the obvious way.
instance GameTree Game where
    terminal g = null (future g) || checkmate g
    children = future
    evaluate = eval

-- | Suggest one of the game's children
suggest :: Algorithm -> Int -> Game -> Maybe Game
suggest a n g = case suggestions a n MaximizingPlayer g of
    []  -> Nothing
    x:_ -> Just x

-- | The type used by all the tree serarch algorithms.
type Algorithm = Int -> Player -> Game -> Score

-- | All games in a game's future, sorted by their score when searched to
-- the passed depth.
suggestions :: Algorithm -> Int -> Player -> Game -> [Game]
suggestions a n p = sortBy (comparing $ a n p) . children
