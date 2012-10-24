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
isPalindrome''' xs = foldr (&&) True (zipWith (==) xs $ myReverse xs) 

{-problem 7-}
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten ( Elem a ) = [a]
flatten ( List [] ) = []
flatten ( List (x:xs) ) = ( flatten x ) ++ flatten (List xs)

{-problem 8-}
compress :: Eq a => [a] -> [a]
compress = foldl f []
    where f [] a = [a]
          f l a
            | (last l) == a  = l
            | otherwise      = l ++ [a]
