-- |
-- Module      : Game.SlowChess.AI.Negascout
-- Description : Negascout tree search algorithm
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A negascout search algorithm.

module Game.SlowChess.AI.Negascout where

import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Functor.Identity

import           Game.SlowChess.AI.Internal

-- | Search states, as used by negascout.
data SearchState = SearchState { _depth  :: Int
                               , _alpha  :: Score
                               , _beta   :: Score
                               , _score  :: Score
                               , _player  :: Player
                               } deriving (Show, Eq)

-- newtype Search a = Search { runSearch :: State SearchState a }
                --    deriving ( Functor, Applicative, Monad )

type Search = StateT SearchState Identity

-- | Positive infinity as a score.
infinity :: Score
infinity = 1 / 0

-- | Turns a player into the sign needed by the algorithm.
val :: Player -> Score
val MaximizingPlayer = 1
val MinimizingPlayer = -1


nega :: GameTree g => (g, Int, Score, Score, Player) -> Score
nega (node, d, α, β, p) =
    if terminal node || d == 0
    then val p * evaluate node
    else getAlpha startState $ do
        let (first:rest) = children node
        depth  <- gets _depth
        player <- gets _player
        setScore $ negate (nega (first, depth-1, -β, -α, other player))
        fancyFor rest $ \ child -> do
            alpha <- gets _alpha
            beta  <- gets _beta
            setScore $ negate (nega (child, depth-1, -(alpha-1), -alpha, other player))
            score <- gets _score
            when (alpha < score && score < beta) $
                setScore $ -nega(child, depth-1, -beta, -score, other player)
            score' <- gets _score
            setAlpha $ max alpha score'
            alpha' <- gets _alpha
            return (alpha' < beta) -- Do we break?
  where startState = SearchState { _depth = d
                                 , _player = p
                                 , _alpha = α
                                 , _beta  = β
                                 , _score = -infinity
                                 }

getAlpha :: SearchState -> State SearchState a -> Score
getAlpha start s = _alpha . snd $ runState s start

setAlpha :: Score -> State SearchState ()
setAlpha alpha' = modify (\s -> s { _alpha = alpha' })

setScore :: Score -> State SearchState ()
setScore score' = modify (\s -> s { _score = score' })

fancyFor :: [g] -> (g -> Search Bool) -> Search Bool
fancyFor [] f = error "fuck"
fancyFor (g:[]) f = f g
fancyFor (g:gs) f = case runState (f g) of
                        (x,y) -> _    

-- | Alpha Beta Pruning tree search.
search :: GameTree g => Int -> Player -> g -> Score
search = undefined
