-- |
-- Module      : Game.SlowChess.AI.Negamax
-- Description : Negamax tree search algorithm
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- MinMax tree search inplementation. I did this one first since it's the most
-- straight forward.

module Game.SlowChess.AI.Negamax where

import           Game.SlowChess.AI.Internal

-- | Negamax tree search.
nega :: GameTree g => Int -> Player -> g -> Score
nega d p g = if d == 0 || terminal g
                then evaluate g
                else maximum scoredChildren
  where scoredChildren = map (negate . nega (pred d) (enemy p)) (children g)
