module Tree
    ( Tree(..)
    , NodeInfo(..)
    , treeleaf
    , toNodeInfo
    , arbitrary
    , shrink
    , coarbitrary
    )
where

import Data.Foldable
import Data.Monoid
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving Eq

-- | Generates an arbitrary tree with a specified number of nodes
genSizedTree :: Arbitrary a => Int -> Gen (Tree a)
genSizedTree 0 = return Empty
genSizedTree n = do leftSize <- choose (0, (n-1))
                    let rightSize = n - leftSize - 1
                    liftM3 Branch arbitrary (genSizedTree leftSize) (genSizedTree rightSize)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized genSizedTree
    shrink Empty = []
    shrink (Branch _ Empty Empty) = [Empty]
    shrink (Branch a l r) = do leftShrink  <- adjust $ shrink l
                               rightShrink <- adjust $ shrink r
                               [ (Branch a leftShrink r),
                                 (Branch a l rightShrink) ]
                            where adjust [] = [Empty] -- always have at least one "Empty"
                                  adjust l  = l

instance CoArbitrary (Tree a) where
    coarbitrary t gen = variant (countNodes t) gen

data NodeInfo a = NodeInfo { value :: a
                           , index :: Int  
                           , depth :: Int  
                           } deriving (Show)

toNodeInfo :: Tree a -> ([NodeInfo a], Int) -- modeinfo list, next index
toNodeInfo t = toNodeInfo' t 0 0
         where toNodeInfo' Empty index _ = ([], index)
               toNodeInfo' (Branch a l r) index depth = 
                           (completeList, lastIndex)
                           where (leftList, leftIndex) = toNodeInfo' l index (depth+1)
                                 thisNodeInfo = (NodeInfo a (leftIndex) depth)
                                 (rightList, lastIndex) = toNodeInfo' l (leftIndex+1) (depth+1)
                                 completeList = leftList ++ thisNodeInfo:rightList

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

-- | Count the number of nodes in a tree
countNodes :: Tree a -> Int
countNodes = getSum . foldMap (\_ -> Sum 1)

