{-# LANGUAGE AllowAmbiguousTypes, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications #-}
module Data.Sparse.Shape where

import Data.Semigroup

data Shape = L | B Shape Shape
  deriving (Eq, Ord, Show)


instance Semigroup Shape where
  (<>) = B


class Size (s :: Shape) where
  size :: Num n => n

instance Size 'L where
  size = 1

instance (Size s1, Size s2) => Size ('B s1 s2) where
  size = size @s1 + size @s2
