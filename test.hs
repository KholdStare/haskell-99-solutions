import Data.List

fac 0 = 1
fac x = x * fac (x - 1)

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
encode :: ( Eq a ) => [a] -> [(Int, a)]
encode = foldr countDups []
    where countDups a [] = [(1, a)]
          -- ^ countDups turns duplicate consecutive elements into
          -- a tuple with that element and count
          countDups a tuples
            | a == b     = (n+1, b):(tail tuples)
            | otherwise  = (countDups a []) ++ tuples
                where tuple = head tuples
                      b     = snd tuple
                      n     = fst tuple

{-problem 9-}
pack :: Eq a => [a] -> [[a]]
pack = ( map extend ) . encode
    where extend (n, x) = map (\_ -> x) [1..n]
          -- ^ extend takes a tuple with an element and count, and
          -- converts it to a list with n elements a

{-problem 11-}
data RunLength a = Single a | Multiple Int a
    deriving Show

encodeModified :: (Eq a) => [a] -> [ RunLength a ]
encodeModified = map convert . encode
    where convert (n, a)
                | n == 1    = Single a
                | otherwise = Multiple n a

{-problem 12-}
decodeModified :: [ RunLength a ] -> [a]
-- | Realized that "foldl (++) [] . map" is concatMap,
-- | but will keep it because it looks sophisticated :P
decodeModified = foldl' (++) [] . map expand
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
removeAt :: Int -> [a] -> (a, [a])
removeAt n l = (removed, rest)
    where (start, end)  = splitAt n l
          removed = last start
          rest = init start ++ end
