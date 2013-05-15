module Main
where

import Control.Monad (liftM)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List (nub)

import Test.QuickCheck

import Problems1_10
import Problems51_60
import Tree

main = defaultMain tests

tests = [
        testGroup "NodeInfo" [
                testProperty "increasing indeces" prop_toNodeInfo_increasingIndex,
                testProperty "intersperse size" prop_intersperse_length
            ],
        testGroup "Problem 5 - reverse" [
                testProperty "concatenate" prop_reverse_concat
                -- TODO add more
            ],
        testGroup "Problem 6 - isPalindrome" [
                testProperty "impl 1" $ prop_isPalindrome_test isPalindrome,
                testProperty "impl 2" $ prop_isPalindrome_test isPalindrome',
                testProperty "impl 3" $ prop_isPalindrome_test isPalindrome'',
                testProperty "impl 4" $ prop_isPalindrome_test isPalindrome'''
                -- TODO add more
            ],
        -- TODO: add tests for some problems
        testGroup "Problem 59" [
                testProperty "no duplicates" prop_hbalTree_noDups,
                testProperty "correct depth" prop_hbalTree_depth
                -- TODO add more
            ],
        testGroup "Problem 60" [
                testProperty "min nodes" prop_hbalMinNodes_nodeCount
            ]
    ]

-- Problems 1 - 10
type TestList = [Int]

prop_reverse_concat :: TestList -> TestList -> Bool
prop_reverse_concat xs ys = (myReverse xs) ++ (myReverse ys) ==
                         (myReverse (ys ++ xs))

prop_isPalindrome_test :: (TestList -> Bool) -> TestList -> Bool
prop_isPalindrome_test impl list = impl evenDrome && impl oddDrome
                        where revlist = reverse list
                              evenDrome = list ++ revlist
                              oddDrome  = list ++ 0:revlist -- add an element in the middle

-- Problems 51 - 60

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
prop_intersperse_length l = length (intersperse () input) == expectedLength
        where extract (n, s) = (getSmallNat n, s)
              input = map extract l
              inputLength = length input
              expectedLength = if inputLength == 0
                                  then 0
                                  else maximum (map fst input) + inputLength

-- max is 4
data ReallySmallNat = ReallySmallNat { getReallySmallNat :: Int } deriving (Show)

instance Arbitrary ReallySmallNat where
    arbitrary = liftM ReallySmallNat $ choose (0, 4)
    shrink = map ReallySmallNat . shrink . getReallySmallNat

prop_hbalTree_noDups :: ReallySmallNat -> Bool
prop_hbalTree_noDups h = length trees == length (nub trees)
        where trees = hbalTree $ getReallySmallNat h

-- TODO make size configurable per test? Also num of tests?
prop_hbalTree_depth :: ReallySmallNat -> Bool
prop_hbalTree_depth h = all (== n) $ map treeDepth trees
        where trees = hbalTree n
              n = getReallySmallNat h

-- could use 5 here?
prop_hbalMinNodes_nodeCount :: ReallySmallNat -> Bool
prop_hbalMinNodes_nodeCount h = minimum (map countNodes trees) == hbalMinNodes height
        where height = getReallySmallNat h
              trees = hbalTree height

