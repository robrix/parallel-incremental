module Data.Higher.Semigroup where

class Semigroup s where
  (<>) :: s a -> s a -> s a
