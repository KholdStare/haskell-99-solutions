module Problems21_30 ( removeAt ) where

import System.Random
import qualified Data.List as DL
import qualified Data.Ord as DO
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap

import Problems11_20

{-problem 21-}
-- | zero-indexed version. Cleaner than 1-based
insertAt :: a -> [a] -> Int -> [a]
insertAt elem l i = start ++ [elem] ++ end
    where (start, end) = splitAt i l

{-problem 22-}
range :: Int -> Int -> [Int]
range i n
    | i == n    = [n]
    | i < n     = i:range (i+1) n
    | i > n     = i:range (i-1) n

{-problem 23-}
rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect l n = do  i <- getStdRandom (randomR (0 :: Int, length l - 1)) 
                    let (elem, remaining) = removeAt i l                 
                    acc <- rndSelect remaining (n-1)                   
                    return ( elem:acc )                                 
-- note to self: replicateM can be used to generate a monadic list of values,
-- which can then be used in list comprehensions!

{-problem 24-}
rndInRange :: Int -> Int -> IO [Int]
rndInRange = flip $ rndSelect  . ( enumFromTo 1 )

{-problem 25-}
rndPerm :: [a] -> IO [a]
rndPerm x@(_:[]) = return x {- not strictly necessary -}
rndPerm l = rndSelect l (length l)

{-problem 26-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n l = do i <- [0..( length l - 1 )]
                      let (x, remaining) = removeAt i l
                      others <- (combinations (n-1) remaining)
                      return ( x:others )

{-problem 27-}
-- not as elegant/efficient a solution as the reference.

genSelVecs :: [Int] -> Set.Set [Int]
-- | given a groupings list, generate a list
-- | of unique selection vectors
-- | e.g. [2,3] -> [[0,0,1,1,1] , ... ]
genSelVecs gs = Set.fromList $ combinations (sum gs) $ concat $ zipWith (replicate) gs groupIDs
        where groupIDs = [0..(length gs - 1)]
-- | "uniqueify-ing" this list is the main bottleneck

insertIntoNth :: a -> Int -> [[a]] -> [[a]]
-- | Given a value and an index n, insert the value
-- | into the nth list
insertIntoNth val 0 (x:xss) = (val:x) : xss
insertIntoNth val n (x:xss) = x : insertIntoNth val (n-1) xss
insertIntoNth val _ _ = [[]]

groupWith :: [Int] -> [a] -> [[a]]
-- | Given a selection vector like [0, 0, 1, 1, 1]
-- | and a list of items, separate the list into groups
-- | using the selection vector
groupWith selVec xs = let numGroups = maximum selVec + 1
                          emptyGroups = replicate numGroups []
                          groupAssignments = zip xs selVec
                          in DL.foldl' insertIntoNth' emptyGroups groupAssignments
                      where insertIntoNth' = flip $ uncurry insertIntoNth

genGroups :: Ord a => [Int] -> [a] -> [[[a]]]
genGroups groupings xs = map ( ( flip groupWith ) xs ) selVecs
        where selVecs = Set.toList $ genSelVecs groupings

tryGenGroups = print $ length $ genGroups [2, 3, 4] "abcdefghi"

{-problem 28-}
lsort :: [[a]] -> [[a]]
-- | Sort the elements of this list according to their length
lsort = DL.sortBy (DO.comparing length)

tryLsort = lsort ["abc","de","fgh","de","ijkl","mn","o"]

lenFrequencies :: [[a]] -> IntMap.IntMap Int
lenFrequencies = DL.foldl' counter IntMap.empty 
    where counter m l = IntMap.insertWith (+) (length l) 1 m
    {-where counter l = undefined-}

tryLenFrequencies = lenFrequencies ["abc","de","fgh","de","ijkl","mn","o"]

lfsort :: [[a]] -> [[a]]
-- | Sort the elements of this list according to their length frequency
lfsort l = let lfs = lenFrequencies l
               lf x = IntMap.findWithDefault 0 (length x) lfs
               in  DL.sortBy (DO.comparing lf) l

tryLfsort = lfsort ["abc","de","fgh","de","ijkl","mn","o"]

