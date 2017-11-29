module Data.Higher.Monoid
( Data.Higher.Monoid.Monoid(..)
) where

import Data.Higher.Semigroup

class Semigroup m => Monoid m where
  mempty :: m a
