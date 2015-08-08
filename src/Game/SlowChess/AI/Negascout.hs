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
data State = State
    { _depth  :: Int
    , _alpha  :: Score
    , _beta   :: Score
    , _player :: Player
    , _score  :: Score
    } deriving (Show, Eq)

-- | Positive infinity as a score.
infinity :: Score
infinity = 1 / 0

-- | Turns a player into the sign needed by the algorithm.
val :: Player -> Score
val MaximizingPlayer = 1
val MinimizingPlayer = -1

nega :: GameTree g => (g, Int, Score, Score, Player) -> Score
nega (node, depth, alpha, beta, player) =
    if terminal node || depth == 0
        then val player * evaluate node
        else
          let first:rest = children node in
          let firstScore = negate $
                nega (first, pred depth, -beta, -alpha, other player) in
          let state = State { _depth  = depth
                            , _alpha  = alpha
                            , _beta   = beta
                            , _player = player
                            , _score  = firstScore} in
          _alpha $ for rest state body

body :: GameTree g => g -> State -> Continue State
body child state@(State { _depth  = depth
                        , _alpha  = alpha
                        , _beta   = beta
                        , _player = player}) =
    let score = negate $
            nega (child, pred depth, -(pred alpha), -alpha, other player) in
    let score' = if alpha < score && score < beta
        then negate $
            nega (child, pred depth, -beta,         -score, other player)
        else negate $
            nega (child, pred depth, -(pred alpha), -alpha, other player) in
    let alpha' = max alpha score'                                         in
    let state' = state { _score = score', _alpha = alpha' }               in
    if alpha' >= beta
        then Break state'
        else Cont  state'


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
search d p g = nega (g, d, infinity, -infinity, p)
