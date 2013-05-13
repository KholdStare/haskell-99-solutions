module Main
where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Tree

main = defaultMain tests

tests = [
        testGroup "NodeInfo" [
                testProperty "increasing indeces" prop_toNodeInfo_increasingIndex
            ]
    ]

prop_toNodeInfo_increasingIndex :: Tree () -> Bool
prop_toNodeInfo_increasingIndex =
    areConsecutiveNumbers . getIndeces . toNodeInfo
    where getIndeces = map index . fst
          areConsecutiveNumbers = all (uncurry (==)) . zip [0..]

