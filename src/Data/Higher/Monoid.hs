module Data.Higher.Monoid
( HMonoid(..)
) where

class HMonoid m where
  hmempty :: m a
  hmappend :: m a -> m a -> m a
