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
import Problems11_20
import Problems51_60
import Tree

main = defaultMain tests

tests = [
        -- TODO look into unit tests?
        testGroup "Helpers" [
                testProperty "keepNth" prop_keepNth_length
            ],
        testGroup "NodeInfo" [
                testProperty "increasing indeces" prop_toNodeInfo_increasingIndex,
                testProperty "intersperse size" prop_intersperse_length
            ],
        testGroup "Problem 5 - reverse" [
                testProperty "concatenate" prop_reverse_concat -- TODO rename
            ],
        testGroup "Problem 6 - isPalindrome" [
                testProperty "impl 1" $ prop_isPalindrome_test isPalindrome,
                testProperty "impl 2" $ prop_isPalindrome_test isPalindrome',
                testProperty "impl 3" $ prop_isPalindrome_test isPalindrome'',
                testProperty "impl 4" $ prop_isPalindrome_test isPalindrome'''
            ],
        testGroup "Problem 8 - compress" [
                testProperty "length doesn't get longer" prop_compress_length,
                testProperty "correct order" prop_compress_firstLast
            ],
        testGroup "Problem 10 - encode (runLength)" [
                testProperty "counts add up to length" prop_encode_count 
            ],
        testGroup "Problem 11-13 - encode/decode runlength modified" [
                testProperty "identity" prop_encode_decode_modified,
                testProperty "both encode functions match" prop_encode_modified_direct
            ],
        testGroup "Problem 14 - duplicate list elements" [
                testProperty "length doubles" prop_dupli_length,
                testProperty "every second val matches original" prop_dupli_dropNth
            ],
        testGroup "Problem 15 - replicate list elements" [
                testProperty "length increases" prop_repli_length,
                testProperty "every nth val matches original" prop_repli_dropNth
            ],
        testGroup "Problem 16 - dropNth value from list" [
                testProperty "length decreases" (prop_dropNth_length dropNth),
                testProperty "length decreases''" (prop_dropNth_length dropNth'')
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

-- Helper types

data SmallNat = SmallNat { getSmallNat :: Int } deriving (Show)

instance Arbitrary SmallNat where
    arbitrary = liftM SmallNat $ choose (0, 100)
    shrink = map SmallNat . shrink . getSmallNat

-- Helper functions

keepNth :: Int -> [a] -> [a]
keepNth n = reverse . keepNth' (n-1) (n-1) []
        where keepNth' _ _ acc []     = acc
              keepNth' 0 n acc (x:xs) = keepNth' n     n (x:acc) xs
              keepNth' i n acc (x:xs) = keepNth' (i-1) n    acc  xs

prop_keepNth_length :: TestList -> SmallNat -> Bool
prop_keepNth_length list num = dropLen * n >= origLen - n &&
                               dropLen * n <= origLen
               where n       = 1 + (getSmallNat num) -- always > 0
                     origLen = length list
                     dropLen = length (keepNth n list)

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

prop_compress_length :: TestList -> Bool
prop_compress_length l = length (compress l) <= length l

prop_compress_firstLast :: TestList -> Bool
prop_compress_firstLast [] = True
prop_compress_firstLast [x] = True
prop_compress_firstLast l = head compl == head l &&
                        last compl == last l
        where compl = compress l

prop_encode_count :: TestList -> Bool
prop_encode_count l = length l == (sum $ map snd $ encode l)

-- Problems 11 - 20

prop_encode_decode_modified :: String -> Bool
prop_encode_decode_modified s = decodeModified (encodeModified s) == s

prop_encode_modified_direct :: String -> Bool
prop_encode_modified_direct s = (encodeDirect s) == (encodeModified s)

prop_dupli_length :: TestList -> Bool
prop_dupli_length l = 2 * (length l) == length (dupli l)

prop_dupli_dropNth :: TestList -> Bool
prop_dupli_dropNth list = keepNth 2 (dupli list) == list

prop_repli_length :: TestList -> SmallNat -> Bool
prop_repli_length list num = n * (length list) == length (repli list n)
                   where n = 1 + (getSmallNat num) -- always > 0

prop_repli_dropNth :: TestList -> SmallNat -> Bool
prop_repli_dropNth list num = keepNth n (repli list n) == list
                    where n = 1 + (getSmallNat num) -- always > 0

-- take implementation to test as first param
prop_dropNth_length :: ([Int] -> Int -> [Int]) -> TestList -> SmallNat -> Bool
prop_dropNth_length fun list num = origLen < n ||
                                   ( dropLen * n > origLen * (n-1) - n &&
                                     dropLen * n < origLen * (n-1) + n )
                   where n       = 1 + (getSmallNat num) -- always > 0
                         origLen = length list
                         dropLen = length (fun list n)
-- Problems 51 - 60

prop_toNodeInfo_increasingIndex :: Tree () -> Bool
prop_toNodeInfo_increasingIndex =
    areConsecutiveNumbers . getIndeces . toNodeInfo
    where getIndeces = map index . fst
          areConsecutiveNumbers = all (uncurry (==)) . zip [0..]

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

