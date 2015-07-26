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
import           Game.SlowChess.Game.Internal
import           Game.SlowChess.Evaluate

import           Data.List                  (sortBy)
import           Data.Ord                   (comparing)

-- | Chess games form a 'GameTree', in the obvious way.
instance GameTree Game where
    terminal g = null (future g) || checkmate g
    children = future
    evaluate = eval

-- | Suggest one of the game's children
suggest :: Int -> Game -> Maybe Game
suggest n g = case suggestions n g of
                []  -> Nothing
                x:_ -> Just x

-- | All games in a game's future, sorted by their score when searched to
-- the passed depth.
suggestions :: Int -> Game -> [Game]
suggestions n = sortBy (comparing $ nega n MaximizingPlayer) . children
