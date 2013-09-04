primes :: [Int]
primes = 2 : [ x | x <- [3,5..], isPrime x]

doesntDivide :: Int -> Int -> Bool
doesntDivide x by = x `mod` by > 0

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

upTo :: [Int] -> Int -> [Int]
upTo list limit = takeWhile ( (>=) limit ) list

isPrime x = all (doesntDivide x) $ primes `upTo` sqrt' x

main = putStrLn $ unlines $ map show $ take 100000 primes
