-- |
-- Module      : Game.SlowChess.REPL
-- Description : Tools for building a repl with Haskeline.
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Tools for building a REPL for configuring and playing chess games from
-- a command line.

module Game.SlowChess.REPL where

import           System.Console.Haskeline

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Error

import qualified Game.SlowChess.AI            as AI
import qualified Game.SlowChess.AI.Negamax    as Max   (search)
import qualified Game.SlowChess.AI.Negascout  as Scout (search)
import           Game.SlowChess.Game
import           Game.SlowChess.Game.Internal

-- | The different search algorithms available.
data AlgorithmName = Negamax | Negascout

-- | The configuration used by the game.
data Config = Config { algoName :: AlgorithmName
                     , depth    :: Int
                     }

defaultConfig :: Config
defaultConfig = self
  where self = Config Negamax 2

algorithm :: Config -> Int -> AI.Player -> Game -> AI.Score
algorithm c = case algoName c of
    Negamax   -> Max.search
    Negascout -> Scout.search

helpMessage :: String
helpMessage = "No help yet."

mainMenu :: InputT IO ()
mainMenu = do
    input <- getInputLine "slowchess > "
    case input of
        Nothing -> return ()
        Just "quit" -> void $ outputStrLn "bye"
        Just "exit" -> void $ outputStrLn "bye"
        Just "help" -> outputStrLn helpMessage
        Just "new"  -> gameStarter defaultConfig
        Just cmd    -> outputStrLn msg >> mainMenu
          where msg = "Command '" ++ cmd ++ "' not recognized. Try 'help'."

gameStarter :: Config -> InputT IO ()
gameStarter config = do
    whitePlayer <- getPlayer "white" config
    blackPlayer <- getPlayer "black" config
    result <- runErrorT $ playGame whitePlayer blackPlayer start
    case result of
        Left msg -> outputStrLn msg
        Right g  -> error $ "the game isn't over: " ++ show g

type Player = Game -> ErrorT Result (InputT IO) Game

type Result = String

getPlayer :: String -> Config -> InputT IO Player
getPlayer name config = do
    input <- getInputLine $ "Who will play " ++ name ++ "? [human/cpu]: "
    case input of
        Just "human" -> return $ humanPlayer config
        Just "cpu"   -> return $ cpuPlayer config
        _            -> do outputStrLn "That's not an option."
                           getPlayer name config

humanPlayer :: Config -> Player
humanPlayer = error "Human players not yet supported."

cpuPlayer :: Config -> Player
cpuPlayer c g = case AI.suggest (algorithm c) (depth c) g of
    Nothing -> throwError "No legal game"
    Just g' -> return g'

playGame :: Player -> Player -> Game -> ErrorT Result (InputT IO) Game
playGame white black = go
  where printGame = liftIO . putStr . show
        go g = do printGame g
                  g'  <- white g
                  printGame g'
                  g'' <- black g'
                  if checkmate g'' || draw g''
                      then -- TODO: Write a better message.
                           throwError "The game is over."
                      else go g''
