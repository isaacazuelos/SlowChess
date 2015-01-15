module Main where

import           Game.SlowChess.Board
import           Game.SlowChess.Mask
import           Game.SlowChess.Movement
import           Game.SlowChess.Piece
import           Game.SlowChess.Pretty

import Data.Monoid ((<>))

main :: IO ()
main = do
  let b = blank { kings  = fromList [27]
                , blacks = fromList [27]
                , whites = fromList [34]
                , rooks  = fromList [34]
                }
  pprint b
  pprint (blanks b <> material (enemy Black) b)
  mapM_ pprint $ moveKings Black b
