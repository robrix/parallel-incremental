module Data.Sparse.Shape where

import Data.Semigroup

data Shape = L | B Shape Shape
  deriving (Eq, Ord, Show)


instance Semigroup Shape where
  (<>) = B
