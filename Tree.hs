module Tree ( Tree(Empty, Branch), treeleaf ) where

import Data.Foldable
import Data.Monoid

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving Eq

-- TODO: rotate it
instance Show a => Show (Tree a) where
    show t = unlines $ show' t 0
        where show' (Empty) indent = []
              show' (Branch val l r) indent = right ++ self ++ left
                        where right = show' r (indent+1)
                              self = [ replicate indent '\t' ++ (show val) ]
                              left = show' l (indent+1)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Branch a l r) = foldMap f l `mappend` f a `mappend` foldMap f r

treeleaf a = Branch a Empty Empty

-- | Count the numbe of nodes in a tree
countNodes :: Tree a -> Int
countNodes = getSum . foldMap (\_ -> Sum 1)
