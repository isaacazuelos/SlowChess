-- |
-- Module      : Game.SlowChess.AI.MinMax
-- Description : MinMax tree search algorithm.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- MinMax tree search inplementation. I did this one first since it's the
-- most straight forward.

module Game.SlowChess.AI.MinMax where

import           Game.SlowChess.AI.Internal

-- | A minmax-like tree search.
minmax :: GameTree g => Int -> g -> Score
minmax d g
    | d == 0 || terminal g = score g
    | otherwise = maximum $ map (minmax (pred d)) (children g)
