module Main where

import Criterion.Main

import Game.SlowChess.AI
import Game.SlowChess.Game

main :: IO ()
main = defaultMain [ benchNegamax 3 ]

benchNegamax :: Int -> Benchmark
benchNegamax n = bench ("Negamax to " ++ show n) $ whnf (suggest n) start

benchDepth :: Int -> Benchmark
benchDepth n = bench ("toDepth " ++ show n) $ whnf (suggest n) (toDepth n start)
