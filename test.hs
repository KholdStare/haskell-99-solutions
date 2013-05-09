import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.Ord as DO

import Problems1_10
import Problems11_20
import Problems21_30
import Problems31_40


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
