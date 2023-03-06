module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Tier0.Writer (Tree (..))

first (a,b) = a
second (a,b) = b
collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf x) = writer ([x],x)
collectAndSumInOrder (Branch l v r) = 
  writer $ (first (runWriter (collectAndSumInOrder l)),second (runWriter (collectAndSumInOrder l)))

 
  