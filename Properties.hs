module Main
where

import Data.Monoid (mempty)
import Control.Monad (liftM)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Tree

main = defaultMain tests

tests = [
        testGroup "NodeInfo" [
                testProperty "increasing indeces" prop_toNodeInfo_increasingIndex,
                testProperty "intersperse size" prop_intersperse_length
            ]
    ]

prop_toNodeInfo_increasingIndex :: Tree () -> Bool
prop_toNodeInfo_increasingIndex =
    areConsecutiveNumbers . getIndeces . toNodeInfo
    where getIndeces = map index . fst
          areConsecutiveNumbers = all (uncurry (==)) . zip [0..]

data SmallNat = SmallNat { getSmallNat :: Int } deriving (Show)

instance Arbitrary SmallNat where
    arbitrary = liftM SmallNat $ choose (0, 100)
    shrink = map SmallNat . shrink . getSmallNat

prop_intersperse_length :: [ (SmallNat, ()) ] -> Bool
prop_intersperse_length l = (length $ intersperse () input) == expectedLength
        where extract (n, s) = (getSmallNat n, s)
              input = map extract l
              inputLength = length input
              expectedLength = if inputLength == 0
                                  then 0
                                  else (maximum $ map fst $ input) + inputLength
