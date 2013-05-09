import qualified Data.List as DL

import Problems1_10
import Problems11_20
import Problems21_30
import Problems31_40
import Problems41_50


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
