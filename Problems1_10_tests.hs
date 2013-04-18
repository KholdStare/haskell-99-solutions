#!/usr/bin/runhaskell
import Problems1_10
import Test.QuickCheck
import Control.Monad

type TestList = [Int]

prop_reverse :: TestList -> Bool
prop_reverse l = ( myReverse . myReverse ) l == l

-- how to use this property?
{-prop_reverse_add xs ys = (myReverse xs) ++ (myReverse ys) ==-}
                         {-(myReverse (ys ++ xs))-}

prop_length :: TestList -> Bool
prop_length l = myLength l == length l

{-doAll :: (a -> IO b) -> [a] -> IO ()-}
{-doAll _ [] = return ()-}
{-doAll fun (x:xs) = fun x >> doAll fun xs-}
                         
main = quickCheck prop_reverse
       >> quickCheck prop_length
