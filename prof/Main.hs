module Main where

import Game.SlowChess.AI
import Game.SlowChess.Game

main :: IO ()
main = print $ suggest 3 start
