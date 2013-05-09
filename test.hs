import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.Ord as DO

import Problems1_10
import Problems11_20
import Problems21_30

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
              deriving Eq

instance Show a => Show (Tree a) where
    show t = unlines $ show' t 0
        where show' (Empty) indent = []
              show' (Branch val l r) indent = right ++ self ++ left
                        where right = show' r (indent+1)
                              self = [ replicate indent '\t' ++ (show val) ]
                              left = show' l (indent+1)


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
hbalTree :: Int -> [ Tree () ]
hbalTree 0 = [ Empty ]
hbalTree n = do treeSameHeight1 <- cbalTree (n-1)
                treeSameHeight2 <- cbalTree (n-1)
                treeSmallHeight <- cbalTree (n-2)
                let unbalanced = [ Branch () treeSameHeight1 treeSmallHeight,
                                   Branch () treeSmallHeight treeSameHeight1 ] 
                let balanced = [ Branch () treeSameHeight1 treeSameHeight2 ]
                unbalanced ++ balanced

-- | Inserts a given value after each element in a list
alternateWith :: a -> [a] -> [a]
alternateWith a [] = []
alternateWith a (x:xs) = x:a:alternateWith a xs
