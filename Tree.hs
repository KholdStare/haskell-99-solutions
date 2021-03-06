module Tree
    ( Tree(..)
    , NodeInfo(..)
    , leaf
    , toNodeInfo
    , arbitrary
    , shrink
    , coarbitrary
    , countNodes
    , treeDepth
    -- helpers
    , intersperse
    )
where

import Data.Foldable (Foldable, foldMap)
import Data.Function
import Data.Monoid
import Data.Maybe
import Data.List (sortBy, groupBy)
import Data.Ord
import Control.Monad
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Tree as DT

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving Eq

leaf :: a -> Tree a
leaf a = Branch a Empty Empty

toRoseTree :: Tree a -> Maybe (DT.Tree a)
toRoseTree Empty = Nothing
toRoseTree (Branch a l r) = Just $ DT.Node a leaves
    where leaves = catMaybes $ map toRoseTree $ [l, r]


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
                                  adjust lst = lst

instance CoArbitrary (Tree a) where
    coarbitrary t gen = variant (countNodes t) gen

data NodeInfo a = NodeInfo { value :: a
                           , index :: Int  
                           , depth :: Int  
                           } deriving (Show)

-- | Convert tree into in-order tranversal list of values
-- along with the index/depth of the nodes
toNodeInfo :: Tree a -> ([NodeInfo a], Int) -- nodeinfo list, next index
toNodeInfo t = toNodeInfo' t 0 0
         where toNodeInfo' Empty ind _ = ([], ind)
               toNodeInfo' (Branch a l r) ind depth = 
                           (completeList, lastIndex)
                           where (leftList, leftIndex) = toNodeInfo' l ind (depth+1)
                                 thisNodeInfo = (NodeInfo a (leftIndex) depth)
                                 (rightList, lastIndex) = toNodeInfo' l (leftIndex+1) (depth+1)
                                 completeList = leftList ++ thisNodeInfo:rightList

-- given a filler element, intersperse it amongst a "sparse" list
-- specified by (index,value) pairs
intersperse :: a -> [(Int, a)] -> [a]
intersperse filler = intersperse' 0 filler . sortBy (comparing fst)
        where intersperse' _ _ [] = []
              intersperse' i filler all@((index, val):rest) =
                         if i == index
                            then val:intersperse' (i) filler rest
                            else filler:intersperse'     (i+1) filler all

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show t = unlines $ linesByDepth
        where nodesSortedByDepth = sortBy (comparing depth `thenComparing` index) $ fst $ toNodeInfo t
              nodesPerDepth      = groupBy ((==) `on` depth) nodesSortedByDepth -- :: [[NodeInfo]]
              linesByDepth = map rowToString nodesPerDepth
              rowToString = concat . intersperse "\t" . map (\nodeinfo -> (index nodeinfo, show $ value nodeinfo))

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Branch a l r) = foldMap f l `mappend` f a `mappend` foldMap f r

-- | Count the number of nodes in a tree
countNodes :: Tree a -> Int
countNodes = getSum . foldMap (\_ -> Sum 1)

-- | Count the number of nodes in a tree
treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Branch _ l r) = max (treeDepth l) (treeDepth r) + 1

-- Ord Helpers

-- Use as an infix operator
thenComparing :: (Ord b) => (a -> a -> Ordering)
                         -> (a -> b) -- property of a to compare with
                         -> (a -> a -> Ordering)
thenComparing comparator property a1 a2 =
         case comparator a1 a2 of
              EQ -> compare (property a1) (property a2)
              otherwise -> otherwise
