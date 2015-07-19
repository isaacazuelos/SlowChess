-- |
-- Module      : Game.SlowChess.AI.MinMax
-- Description : MinMax tree search algorithm.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- MinMax tree search inplementation. I did this one first since it's the
-- most straight forward.

module Game.SlowChess.AI.MinMax where

import           Data.Function              (on)
import           Data.List                  (maximumBy)

import           Game.SlowChess.AI.Internal

minmax :: GameTree g => Int -> g -> (g, Score)
minmax d g
    | d == 0 || terminal g = score g
    | otherwise = maximumBy (compare `on` snd) $
                    map (minmax (pred d)) (children g)
