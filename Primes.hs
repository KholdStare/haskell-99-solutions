-- helpers
doesntDivide :: Int -> Int -> Bool
doesntDivide x by = x `mod` by > 0

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

upTo :: [Int] -> Int -> [Int]
upTo list limit = takeWhile ( (>=) limit ) list

-- Lazy list implementation
primes :: [Int]
primes = 2 : [ x | x <- [3,5..], isPrime x]

isPrime x = all (doesntDivide x) $ primes `upTo` sqrt' x

main = putStr $ unlines $ map show $ take 100000 primes
