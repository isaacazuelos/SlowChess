module Main where

import           Game.SlowChess.Board
import           Game.SlowChess.Movement
import           Game.SlowChess.Piece
import           Game.SlowChess.Pretty

main :: IO ()
main = putStrLn . concatMap pretty $ moves White Pawn (Rank Down) starting
