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
data State = S
    { depth  :: Int
    , alpha  :: Score
    , beta   :: Score
    , player :: Player
    , score  :: Score
    } deriving (Show, Eq)

-- | Positive infinity as a score.
infinity :: Score
infinity = 1 / 0

-- | Turns a player into the sign needed by the algorithm.
val :: Player -> Score
val MaximizingPlayer = 1
val MinimizingPlayer = -1

nega :: GameTree g => State -> g -> Score
nega s node =
    if terminal node || depth s == 0
        then val (player s) * evaluate node
        else
          let first:rest = children node in
          let startScore = -(nega s first) in
          let s' = S (depth s - 1) (-(alpha s) - 1) (-alpha s) (other (player s)) (error "huh?") in
          if null rest
              then startScore
              else alpha $ for rest s' body
                 where body = undefined
--         score = -pvs(child, depth-1, -alpha-1, -alpha, -color)
--         if alpha < score < beta
--             score = -pvs(child, depth-1, -beta, -score, -color)
--         alpha = max(alpha, score)
--         if alpha >= beta:
--             break

data Continue_ a b = Break a | Cont b deriving (Show, Eq)

type Continue s = Continue_ s s

value :: Continue s -> s
value (Cont  s) = s
value (Break s) = s

for :: [a] -> s -> (a -> s -> Continue s) -> s
for col state f = value (go col (Cont state))
  where go  [] s = s
        go  _ (Break s) = Break s
        go (c:cs) (Cont s) = go cs (f c s)


-- | Alpha Beta Pruning tree search.
search :: GameTree g => Int -> Player -> g -> Score
search = undefined
