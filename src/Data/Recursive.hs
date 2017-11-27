module Data.Recursive
( Recursive(..)
) where

class Recursive m where
  mu :: (m a -> m a) -> m a
