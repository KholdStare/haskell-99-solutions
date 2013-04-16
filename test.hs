import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.Ord as DO
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import System.Random

{-problem 1-}
myLast :: [a] -> a
myLast [e] = e
myLast l = myLast (tail l)

{-note: secondGuy can is same as (const id) or (flip const) or (curry snd) -}
secondGuy :: a -> b -> b
secondGuy a b = b

myLast' :: [Int] -> Int
myLast' = foldr1 ( secondGuy )

{-problem 2-}
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse

{-problem 3-}
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt _ 0 = error "Cannot get zeroth element"
elementAt ( x:_ ) 1 = x
elementAt ( x:xs ) n = elementAt xs ( n - 1 )

{-problem 4-}
myLength :: [a] -> Int
myLength = foldl ( \x -> (\_ -> x + 1) ) 0

myLength' :: [a] -> Int
myLength' = foldl (const . (+1)) 0

{-problem 5-}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = ( myReverse xs ) ++ [x]

{-problem 6-}
isPalindrome ::  Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs) = ( x == t ) && (isPalindrome middle)
    where middle = init xs
          t      = last xs

isPalindrome' ::  Eq a => [a] -> Bool
isPalindrome' x = myReverse x == x

isPalindrome'' ::  Eq a => [a] -> Bool
isPalindrome'' = duplicate $ (==) . reverse
    where duplicate f a = f a a

isPalindrome''' ::  Eq a => [a] -> Bool
isPalindrome''' xs = foldl (&&) True (zipWith (==) xs $ myReverse xs) 

{-problem 7-}
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten ( Elem a ) = [a]
flatten ( List [] ) = []
flatten ( List (x:xs) ) = ( flatten x ) ++ flatten (List xs)

{-problem 8-}
compress :: Eq a => [a] -> [a]
compress = foldl skipDups []
    where skipDups [] a = [a]
          skipDups l a
            | (last l) == a  = l
            | otherwise      = l ++ [a]

{-problem 10-}
encode :: ( Eq a ) => [a] -> [(a, Int)]
encode = foldr countDups []
    where countDups a [] = [(a, 1)]
          -- ^ countDups turns duplicate consecutive elements into
          -- a tuple with that element and count
          countDups a tuples
            | a == b     = (b, n+1):(tail tuples)
            | otherwise  = (countDups a []) ++ tuples
                where tuple = head tuples
                      b     = fst tuple
                      n     = snd tuple

{-problem 9-}
pack :: Eq a => [a] -> [[a]]
pack = ( map extend ) . encode
    where extend (x, n) = map (\_ -> x) [1..n]
          -- ^ extend takes a tuple with an element and count, and
          -- converts it to a list with n elements a

{-problem 11-}
data RunLength a = Single a | Multiple Int a
    deriving Show

encodeModified :: (Eq a) => [a] -> [ RunLength a ]
encodeModified = map convert . encode
    where convert (a, n)
                | n == 1    = Single a
                | otherwise = Multiple n a

{-problem 12-}
decodeModified :: [ RunLength a ] -> [a]
-- | Realized that "foldl (++) [] . map" is concatMap,
-- | but will keep it because it looks sophisticated :P
decodeModified = DL.foldl' (++) [] . map expand
    where expand ( Single a )     = [a]
          expand ( Multiple n a ) = replicate n a

{-problem 13-}
encodeDirect :: ( Eq a ) => [a] -> [ RunLength a ]
encodeDirect = map simplify . foldr countDups []
    where countDups a [] = [Multiple 1 a]
          countDups a acc
            | a == b     = (Multiple (n+1) b):(tail acc)
            | otherwise  = (countDups a []) ++ acc
                where Multiple n b = head acc
          simplify ( Multiple 1 a ) = Single a
          simplify a = a

{-problem 14-}
dupli :: [a] -> [a]
dupli = concatMap $ replicate 2

{-problem 15-}
repli :: [a] -> Int -> [a]
repli = flip $ concatMap . replicate

{-problem 16-}
dropNth :: [a] -> Int -> [a]
dropNth l n = dropNth' l n
    where dropNth' [] _     = []
          dropNth' (x:xs) 1 = dropNth' xs n
          dropNth' (x:xs) a = x:(dropNth' xs (a-1))

dropNth'' :: [a] -> Int -> [a]
dropNth'' = flip $ \n -> ( uncurry (++) ) . (process n) . ( splitAt n )
    where process _ ([], []) = ([], [])
          process n (h, t) = (take (n-1) h, dropNth'' t n)

{-problem 17-}
split :: [a] -> Int -> ([a], [a])
split l n = helper l n
    where helper [] _ = ([], [])
          helper l 0 = ([], l)
          helper (x:xs) a = (x:(fst result), snd result)
            where result = helper xs (a-1)

{-problem 18-}
slice :: [a] -> Int -> Int -> [a]
slice l start end = drop (start-1) $ take end l

{-problem 19-}
rotate ::  [a] -> Int -> [a]
rotate l n = uncurry ( flip (++) ) $ splitAt index l 
    where index = mod n (length l)

{-problem 20-}
-- | zero-indexed version.
removeAt :: Int -> [a] -> (a, [a])
removeAt n l = (removed, rest)
    where (start, end)  = splitAt (n+1) l
          removed = last start
          rest = init start ++ end

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
