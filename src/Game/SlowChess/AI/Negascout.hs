-- |
-- Module      : Game.SlowChess.AI.Negascout
-- Description : Negascout tree search algorithm
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A negascout search algorithm.

module Game.SlowChess.AI.Negascout where

import           Game.SlowChess.AI.Internal

-- | Search states, as used by negascout.
data GameTree g => SearchState g a = State { alpha  :: Score
                                           , beta   :: Score
                                           , depth  :: Int
                                           , player :: Player
                                           , value  :: a
                                           } deriving (Show, Eq)

-- | Positive infinity as a score.
infinity :: Score
infinity = 1 / 0

-- function pvs(node, depth, α, β, color)
--     if node is a terminal node or depth = 0
--         return color × the heuristic value of node
--     for each child of node
--         if child is not first child
--             score := -pvs(child, depth-1, -α-1, -α, -color)       (* search with a null window *)
--             if α < score < β                                      (* if it failed high,
--                 score := -pvs(child, depth-1, -β, -score, -color)        do a full re-search *)
--         else
--             score := -pvs(child, depth-1, -β, -α, -color)
--         α := max(α, score)
--         if α ≥ β
--             break                                            (* beta cut-off *)
--     return α




-- | Alpha Beta Pruning tree search.
-- search :: GameTree g => Int -> Player -> g -> Score
-- search d p g = negascout (startState g d p) g
