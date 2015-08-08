-- |
-- Module      : Game.SlowChess.AI
-- Description : The chess AI interface
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This module ties together the game, scoring and tree searching together
-- to provide a way of suggesting

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.SlowChess.AI ( Player ( MaximizingPlayer
                                  , MinimizingPlayer
                                  )
                         , suggest
                         ) where

import           Game.SlowChess.AI.Internal
import           Game.SlowChess.AI.Negascout  (search)
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
suggest :: Int -> Player -> Game -> Maybe Game
suggest n p g = case suggestions n p g of
                  []  -> Nothing
                  x:_ -> Just x

-- | All games in a game's future, sorted by their score when searched to
-- the passed depth.
suggestions :: Int -> Player -> Game -> [Game]
suggestions n p = sortBy (comparing $ search n p) . children
