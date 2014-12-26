module Main where

import           Game.SlowChess.Board
import           Game.SlowChess.Pretty

main :: IO ()
main = do putStrLn "This is what a starting board looks like: "
          putStrLn $ pretty starting
