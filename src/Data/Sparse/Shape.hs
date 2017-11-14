module Data.Sparse.Shape where

data Shape = L | B Shape Shape
  deriving (Eq, Ord, Show)
