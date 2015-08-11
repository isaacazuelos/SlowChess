module Main where

import           System.Console.Haskeline

import           Control.Monad.Trans.Except

import           Game.SlowChess.Game
import           Game.SlowChess.REPL

-- TODO: Have command line args for setting depth, algorithm, etc.
main :: IO ()
main = do
    result <- runInputT intputSettings . runExceptT $ game
    case result of
        Left msg -> print msg
        Right g  -> error $ "unfinished game: " ++ show g
  where intputSettings = setComplete noCompletion defaultSettings
        config = defaultConfig { depth = 10, algoName = Negascout }
        defaultCPU = cpuPlayer config
        game = playGame defaultCPU defaultCPU start
