module Problems11_20 ( removeAt ) where

import Problems1_10

import qualified Data.List as DL

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

