module Main where

import Criterion.Main

import Game.SlowChess.AI
import Game.SlowChess.Game

main :: IO ()
main = defaultMain [benchNegamax]

benchNegamax :: Benchmark
benchNegamax = bench "Negamax to 2" $ whnf (suggest 2) start
