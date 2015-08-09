module Main where

import           Game.SlowChess.AI
import qualified Game.SlowChess.AI.Negascout as Negascout (search)
import           Game.SlowChess.Game

main :: IO ()
main = print $ suggest Negascout.search 15 start
