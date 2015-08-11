module Main where

import           System.Console.Haskeline
import           Game.SlowChess.REPL

-- | Start the main menu.
main :: IO ()
-- TODO: Have command line args for setting depth, algorithm, etc.
main = runInputT settings mainMenu
  where settings = setComplete noCompletion defaultSettings
