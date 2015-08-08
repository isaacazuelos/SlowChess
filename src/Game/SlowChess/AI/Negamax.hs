-- |
-- Module      : Game.SlowChess.AI.Negamax
-- Description : Negamax tree search algorithm
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Negamax tree search inplementation. I did this one first since it's the
-- most straight forward.

module Game.SlowChess.AI.Negamax ( search ) where

import           Game.SlowChess.AI.Internal

-- | Negamax tree search.
search :: GameTree g => Int -> Player -> g -> Score
search d p g = if d == 0 || terminal g
                  then evaluate g
                  else maximum scoredChildren
  where scoredChildren = map (negate . search (pred d) (other p)) (children g)
