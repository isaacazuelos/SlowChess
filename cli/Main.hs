module Main where

import           Game.SlowChess.AI   (suggest)
import           Game.SlowChess.Game (start)

main :: IO ()
main = print . head $ suggest start
