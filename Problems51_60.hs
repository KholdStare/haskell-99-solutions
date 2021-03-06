module Problems51_60
    ( hbalTree
    , hbalMinNodes
    )
where

import Tree

{-problem 55-}
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
binaryInsert Empty a = leaf a
binaryInsert (Branch val l r) a = 
        if a < val
            then Branch val (binaryInsert l a) r
            else Branch val l (binaryInsert r a)

constructBinTree :: Ord a => [a] -> Tree a
constructBinTree = foldl binaryInsert Empty

{-problem 58-}
symCbalTrees = filter symmetric . cbalTree

{-problem 59-}
-- TODO test
hbalTree :: Int -> [ Tree () ]
hbalTree n
    | n <  0    = []
    | n == 0    = [ Empty ]
    | otherwise = balanced ++ unbalanced
            where balanced = do treeSameHeight1 <- hbalTree (n-1)
                                treeSameHeight2 <- hbalTree (n-1)
                                return $ Branch () treeSameHeight1 treeSameHeight2
                  unbalanced = do treeSameHeight  <- hbalTree (n-1)
                                  treeSmallHeight <- hbalTree (n-2)
                                  [ Branch () treeSameHeight treeSmallHeight,
                                    Branch () treeSmallHeight treeSameHeight ] 

-- | Inserts a given value after each element in a list
-- good for printing a list of multi-line strings with separators
alternateWith :: a -> [a] -> [a]
alternateWith a [] = []
alternateWith a (x:xs) = x:a:alternateWith a xs


{-problem 60-}
-- | Given a height, find the minimum number of
-- nodes required to construct a height-balanced tree
-- of that height.
hbalMinNodes :: Int -> Int
hbalMinNodes 0 = 0
hbalMinNodes 1 = 1
hbalMinNodes h = hbalMinNodes (h-1) + hbalMinNodes (h-2) + 1

-- | Given number of nodes N, return the height of the
-- tallest height-balanced tree that can be constructed
-- with N nodes.
hbalMaxHeight :: Int -> Int
hbalMaxHeight n = undefined
