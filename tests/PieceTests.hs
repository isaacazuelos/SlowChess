{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module PieceTests (tests) where

import           Instances             ()

import           Test.Tasty

-- There's nothing really to test...
tests = testGroup "Piece Tests" []
