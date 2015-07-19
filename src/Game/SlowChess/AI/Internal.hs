-- |
-- Module      : Game.SlowChess.AI.Internal
-- Description : Define the typeclasses used by the search algorithms.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Creates a nice typeclass interface to be used by the tree search
-- algorithms.

module Game.SlowChess.AI.Internal where

-- | A game's score. Scores are floats so we can do division on them nicely,
-- as well as floats already having a build-in representation for infinity and
-- negative infinity.
type Score = Float

-- | A game tree is the tree made by a game and it's possible futures.
class GameTree g where
    terminal :: g -> Bool
    score    :: g -> Score
    children :: g -> [g]
