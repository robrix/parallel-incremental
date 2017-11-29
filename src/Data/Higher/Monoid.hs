module Data.Higher.Monoid
( Monoid(..)
) where

import Prelude hiding (Monoid(..))

class Monoid m where
  mempty :: m a
  (<>) :: m a -> m a -> m a
