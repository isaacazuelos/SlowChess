{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Criterion.Main

import Game.SlowChess.AI
import Game.SlowChess.Game
import Game.SlowChess.Game.Internal

import Control.DeepSeq

main :: IO ()
main = defaultMain [ benchDepth 2, benchNegamax 2 ]

benchNegamax :: Int -> Benchmark
benchNegamax n = bench ("Negamax to " ++ show n) $ whnf (suggest n) start

benchDepth :: Int -> Benchmark
benchDepth n = bench ("toDepth " ++ show n) $ nf (toDepth n) start

deriving instance NFData Game
