{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module BoardTests (tests) where

import           Instances             ()

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Monoid           (mempty)

import           Game.SlowChess.Board
import           Game.SlowChess.Mask   (squish, invert, minus)

tests = testGroup "Board Tests" [ testBlank
                                , testGetSet
                                , testSetGet
                                , testUpdate
                                , testMaterial
                                , testBlanks
                                ]

testBlank = testProperty "Blank boards are blank"
    (\ c p -> get c p blank == mempty)

testGetSet = testProperty "Getting after setting does nothing"
    (\ c p b m -> get c p (set c p b m) == m)

testSetGet = testProperty "Setting after getting does nothing"
    (\ c p b -> set c p b (get c p b) == b)

testUpdate = testGroup "Test update" [ testUpdateId,testUpdateGet ]

testUpdateId = testProperty "Updating nothing does nothing"
    (\ c p b -> update c p b 0 0 == b)

testUpdateGet = testProperty "get after updat does what you expect."
    (\ c p b m1 m2 -> get c p (update c p b m1 m2) ==
                        get c p b `squish` m1 `minus` m2)

testMaterial = testProperty "Material after setting on blank"
    (\ c p m -> material c (set c p blank m) == m)

testBlanks = testProperty "blanks are filled after set"
    (\ c p m -> blanks (set c p blank m) == invert m)
