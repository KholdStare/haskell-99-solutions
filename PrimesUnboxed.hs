import Data.Vector.Unboxed 

import Prelude hiding (takeWhile, length, all, last)

-- same helpers
doesntDivide :: Int -> Int -> Bool
doesntDivide x by = x `mod` by > 0

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

upTo :: Vector Int -> Int -> Vector Int
upTo list limit = takeWhile ( (>=) limit ) list

-- using unboxed vectors
isPrime :: Vector Int -> Int -> Bool
isPrime primes x = all (doesntDivide x) $ primes `upTo` sqrt' x

nextPrime :: Vector Int -> Int
nextPrime vec | length vec == 0 = 2
              | length vec == 1 = 3
              | otherwise       = next
                where isPrime' = isPrime vec
                      next     = until (isPrime') (+2) (last vec + 2)

genPrimeVectorOf size = constructN size nextPrime

main = putStr $ unlines $ Prelude.map show $ toList $ genPrimeVectorOf 100000
