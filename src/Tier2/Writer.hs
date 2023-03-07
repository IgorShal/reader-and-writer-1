module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Tier0.Writer (Tree (..))

first (a,b) = a
second (a,b) = b
collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf x) = do
    tell (Sum x)
    return [x]
collectAndSumInOrder (Branch l v r) = do
    x <- collectAndSumInOrder l
    tell (Sum v)
    y <- collectAndSumInOrder r
    return (x++[v]++y)


    

 
  