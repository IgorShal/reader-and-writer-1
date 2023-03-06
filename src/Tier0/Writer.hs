module Tier0.Writer (Tree (..), sumAndTraceInOrder) where

import Control.Monad.Writer

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq

sumAndTraceInOrder :: Num a => Tree a -> Writer [a] a
sumAndTraceInOrder (Leaf x) = writer (x,[x])
sumAndTraceInOrder (Branch l v r) = do
  x <- sumAndTraceInOrder l
  tell $ return v
  y <- sumAndTraceInOrder r
  return $ x + v + y
    
