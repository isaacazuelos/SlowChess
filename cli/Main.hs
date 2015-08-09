module Main where

import           System.Console.Haskeline

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import qualified Game.SlowChess.AI            as AI
import qualified Game.SlowChess.AI.Negascout  as Negascout
import           Game.SlowChess.Game
import           Game.SlowChess.Game.Internal

main :: IO ()
main = do -- runInputT settings mainMenu
    let game = playGame cpuPlayer cpuPlayer start
    result <- runInputT settings . runExceptT $ game
    case result of
        Left msg -> print msg
        Right g  -> error $ "unfinished game: " ++ show g
  where settings = setComplete noCompletion defaultSettings

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
        Just "new"  -> gameStarter
        Just cmd    -> outputStrLn msg >> mainMenu
          where msg = "Command '" ++ cmd ++ "' not recognized. Try 'help'."

gameStarter :: InputT IO ()
gameStarter = do
    white <- getPlayer "white"
    black <- getPlayer "black"
    result <- runExceptT $ playGame white black start
    case result of
        Left msg -> outputStrLn msg
        Right g  -> error $ "the game isn't over: " ++ show g

type Player = Game -> ExceptT Result (InputT IO) Game

type Result = String

getPlayer :: String -> InputT IO Player
getPlayer name = do
    input <- getInputLine $ "Who will play " ++ name ++ "? [human/cpu]: "
    case input of
        Just "human" -> return humanPlayer
        Just "cpu"   -> return cpuPlayer
        _            -> outputStrLn "That's not an option." >> getPlayer name

humanPlayer :: Player
humanPlayer = error "Human players not yet supported."

cpuPlayer :: Player
cpuPlayer g = case AI.suggest Negascout.search 1 g of
    Nothing -> throwE "No legal game"
    Just g' -> return g'

playGame :: Player -> Player -> Game -> ExceptT Result (InputT IO) Game
playGame white black g = do
    printBoard g
    g'  <- white g
    printBoard g'
    g'' <- black g'
    printBoard g''
    if checkmate g'' || drawn g''
        then throwE "The game is over." -- TODO: Write a better message.
        else playGame white black g''
  where printBoard = liftIO . putStr . show . board
