module Data.Recursive
( Recursive(..)
, Embed(..)
) where

class Recursive m where
  mu :: (m a -> m a) -> m a

class Embed t where
  embed :: f (t f) a -> t f a
