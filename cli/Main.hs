module Main where

import           Game.SlowChess.Board
import           Game.SlowChess.Pretty

main :: IO ()
main = putStrLn $ pretty blank
