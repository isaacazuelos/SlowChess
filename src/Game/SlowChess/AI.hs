{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.SlowChess.AI where

import           Game.SlowChess.Game
import           Game.SlowChess.AI.MinMax
import           Game.SlowChess.AI.Internal

instance GameTree Game where
    terminal g = null (future g) || checkmate g
    children = future
    score g = (g, 0)

suggest :: Game -> Game
suggest = fst . minmax 3
