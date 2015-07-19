-- |
-- Module      : Game.SlowChess.AI.Internal
-- Description : Define the typeclasses used by the search algorithms.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Creates a nice typeclass interface to be used by the tree search
-- algorithms.

module Game.SlowChess.AI.Internal where

type Score = Float

class GameTree g where
    terminal :: g -> Bool
    score    :: g -> (g, Score)
    children :: g -> [g]
