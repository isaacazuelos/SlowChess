{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module BoardTests (tests) where

import           Instances             ()

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Monoid           (mempty)

import           Game.SlowChess.Board
import           Game.SlowChess.Mask   (invert)

tests = testGroup "Board Tests" [ testBlank
                                , testGetSet
                                , testSetGet
                                , testUpdate
                                , testConstUpdate
                                , testMaterial
                                , testBlanks
                                ]

testBlank = testProperty "Blank boards are blank"
    (\ c p -> get c p blank == mempty)

testGetSet = testProperty "Getting after setting does nothing"
    (\ c p b m -> get c p (set c p b m) == m)

testSetGet = testProperty "Setting after getting does nothing"
    (\ c p b -> set c p b (get c p b) == b)

testUpdate = testGroup "Test update" [ testUpdateId ]

testUpdateId = testProperty "Updating with id does nothing"
    (\ c p b -> update c p b id == b)

testConstUpdate = testProperty "Update with constant mask is set"
    (\ c p b m -> update c p b (const m) == set c p b m)

testMaterial = testProperty "Material after setting on blank"
    (\ c p m -> material c (set c p blank m) == m)

testBlanks = testProperty "blanks are filled after set"
    (\ c p m -> blanks (set c p blank m) == invert m)
