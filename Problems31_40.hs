module Problems31_40 ( isPrime', divides, goldbach ) where

import qualified Data.List as DL

import Problems1_10

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

