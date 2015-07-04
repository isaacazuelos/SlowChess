{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MaskTests (tests) where

import           Instances             ()

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Data.Monoid           (mempty, (<>))

import           Game.SlowChess.Mask
import           Game.SlowChess.Piece

tests = testGroup "Mask Tests" [ testListFunctions
                               , testDeMorgans
                               , testSubmask
                               , testTotalSubmask
                               , testHops
                               ]

testListFunctions = testProperty "fromList undoes toList"
    (\ m -> m == (fromList . toList) m)

testDeMorgans = testProperty "Test DeMorgan's on both, invert and mappend"
    (\ a b -> invert (both a b) == invert a <> invert b)

testSubmask = testProperty "Submask detects submasks"
    (\ a b -> (a `minus` b) `submask` a)

testTotalSubmask = testProperty "All masks are submasks of a total mask "
    (\ a -> a `submask` invert mempty)

testHops = testGroup "Test hopping in directions"
    [ testCase "hop north" $ hop N b @?= Mask (2^35)
    , testCase "hop south" $ hop S b @?= Mask (2^19)
    , testCase "hop east"  $ hop E b @?= Mask (2^28)
    , testCase "hop west"  $ hop W b @?= Mask (2^26)
    ]
  where b = Mask (2^27)
