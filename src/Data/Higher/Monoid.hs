module Data.Higher.Monoid
( Monoid(..)
) where

import Data.Higher.Semigroup
import Prelude hiding (Monoid(..))

class Semigroup m => Monoid m where
  mempty :: m a
