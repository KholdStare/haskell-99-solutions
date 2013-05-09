module Problems41_50 ( huffman ) where

import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Ord as DO

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
