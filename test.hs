import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.Ord as DO
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import System.Random

import Problems1_10
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

{-problem 31-}
isPrime' :: Int -> Bool
isPrime' = DL.and . predicates
    where predicates x = map (not . isDivisableBy x) [2..bound x]
          isDivisableBy x d = x `mod` d == 0
          bound = fromEnum . sqrt . toEnum

{-problem 32-}
myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = myGCD b $ a `mod` b

{-problem 33-}
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

{-problem 34-}
totient :: Int -> Int
totient 1 = 1
totient x = DL.foldl' f 1 [2..x-1]
    where f acc y = if coprime x y
                    then acc+1
                    else acc

{-problem 35-}

-- first define prime numbers

doesntDivide :: Int -> Int -> Bool
doesntDivide x by = x `mod` by > 0

sqrt' :: Int -> Int
sqrt' = floor.sqrt . fromIntegral

upTo :: [Int] -> Int -> [Int]
upTo list limit = takeWhile ( (>=) limit ) list

primes :: [Int]
primes = 2 : [ x | x <- [3,5..], isPrime x]

isPrime x = all (doesntDivide x) $ primes `upTo` sqrt' x

-- now define prime factors

primeFactors :: Int -> [Int]
primeFactors = primeFactors' primes

divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

primeFactors' :: [Int] -> Int -> [Int]
primeFactors' _ 1 = []
primeFactors' primes@(curPrime:largerPrimes) x =
            if curPrime `divides` x
                then curPrime : primeFactors' primes (x `quot` curPrime)
                else primeFactors' largerPrimes x

{-problem 36-}
primeFactorsMult :: Int ->  [ (Int, Int) ]
primeFactorsMult = encode . primeFactors

{-problem 37-}
pow a n = pow' 1 a n
    where pow' acc a 0 = acc
          pow' acc a n = pow' (acc*a) a (n-1)

totient' :: Int -> Int
totient' = product . map process . primeFactorsMult
    where process (a, n) = (a-1) * (a `pow` (n-1))

{-problem 39-}
primesR l u = takeWhile ((>=) u) $ dropWhile ((>) l) primes

{-problem 40-}
goldbach x = do prime <- DL.find (isPrime . ((-) x) ) $ primes `upTo` (x `quot` 2)
                let diff = x - prime
                return (prime, diff)

{-problem 41-}
goldbachList l u = DM.catMaybes [ g | num <- [l..u], isEven num,
                                      let g = goldbach num ]
                   where isEven = divides 2

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' l u minPrime = filter ((<) minPrime . fst) $ goldbachList l u

{-problem 46-}
and' True True = True
and' _    _    = False

or' False False = False
or' _     _     = True

not' True = False
not' False = True

nand' a b = not' $ and' a b

nor' a b = not' $ or' a b

xor' True False = True
xor' False True = True
xor' _     _    = False

impl' True False = False
impl' _    _     = True

equ' a b = not' $ xor' a b

-- construct a true/false table for binary boolean function
table :: (Bool -> Bool -> Bool) -> [[Bool]]
table fun = do a <- [True,False]
               b <- [True,False]
               return [ a, b, fun a b ]

-- output said table to stdio
tableIO :: (Bool -> Bool -> Bool) -> IO ()
tableIO = printTable . table

printTable :: Show a => [[a]] -> IO ()
printTable = putStr . unlines . map (unwords . map show)


{-problem 47-}
infixl 4 `or'`
infixl 6 `and'`

{-problem 48-}
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n fun = do bools <- genBools n
                  return $ bools ++ [fun bools]
            where genBools 0 = [[]]
                  genBools n = do b <- [True,False]
                                  rest <- genBools (n-1)
                                  return $ b : rest

tablenIO n = printTable . tablen n 

{-problem 49-}
gray 0 = [ "" ]
gray n = do symbol <- alphabet
            rest <- gray (n-1)
            return $ symbol : rest
         where alphabet = [ '0', '1' ]
            
{-problem 50-}

-- also stores the frequencies as well
data HuffTree a = Leaf Int a | Node Int (HuffTree a) (HuffTree a)

huffleaf (f, a) = Leaf f a

hufffreq :: HuffTree a -> Int
hufffreq ( Leaf f _ ) = f
hufffreq ( Node f _ _ ) = f

union :: HuffTree a -> HuffTree a -> HuffTree a
union h1 h2 = Node (hufffreq h1 + hufffreq h2) h1 h2

huffToAlphabet' :: HuffTree a -> (Char, Char) -> String -> [ (a, String) ]
huffToAlphabet' (Leaf _ a) _ epsilon = [ (a, epsilon) ]
huffToAlphabet' (Node _ left right) alpha@(a0, a1) epsilon =
                    (huffToAlphabet' left alpha ( a0:epsilon )) ++ 
                    (huffToAlphabet' right alpha ( a1:epsilon ))


-- | Given an alphabet of symbols with their frequencies,
-- construct the huffman encoding of such an alphabet
huffman :: [ (Int, a) ] -> [ (a, String) ]
huffman alphabet = huffToAlphabet' (collapse huffleaves) ('0', '1') ""
    where huffleaves = map huffleaf $ DL.sortBy (DO.comparing fst) alphabet
          collapse [tree] = tree
          collapse (t1:t2:rest) = collapse $ DL.insertBy (DO.comparing hufffreq) (union t1 t2) rest

{-problem 55-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

treeleaf a = Branch a Empty Empty

cbalTree :: Int -> [ Tree () ]
cbalTree 0 = [ Empty ]
cbalTree n = do ltree <- cbalTree lsize
                rtree <- cbalTree rsize
                let tree = Branch () ltree rtree
                let tree2 = Branch () rtree ltree
                if lsize == rsize
                    then return tree
                    else [ tree, tree2 ]
            where lsize = (n - 1) `quot` 2
                  rsize = n - lsize - 1

{-problem 56-}

-- checks if two trees are mirrors of each other
-- structurally
symmetric' :: Tree a -> Tree a -> Bool
symmetric' Empty Empty = True
symmetric' (Branch _ l1 r1) (Branch _ l2 r2) =
                (symmetric' l1 r2) && (symmetric' r1 l2)
symmetric' _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = symmetric' l r

{-problem 57-}
binaryInsert :: Ord a => Tree a -> a -> Tree a
binaryInsert Empty a = treeleaf a
binaryInsert (Branch val l r) a = 
        if a < val
            then Branch val (binaryInsert l a) r
            else Branch val l (binaryInsert r a)

constructBinTree :: Ord a => [a] -> Tree a
constructBinTree = foldl binaryInsert Empty

{-problem 58-}
symCbalTrees = filter symmetric . cbalTree

{-problem 59-}
-- TODO: bleh
{-hbalTree :: Int -> [ Tree () ]-}
{-hbalTree 0 = [ Empty ]-}
{-hbalTree n = do tree1 <- cbalTree (n-1)-}
                {-tree2 <- cbalTree (n-2)-}
                {-[ Branch () tree1 tree2 ]-}
                {-if lsize == rsize-}
                    {-then return tree-}
                    {-else [ tree, tree2 ]-}
