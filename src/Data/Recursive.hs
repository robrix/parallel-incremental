{-# LANGUAGE FunctionalDependencies #-}
module Data.Recursive
( Recursive(..)
, Embed(..)
) where

class Recursive m where
  mu :: (m a -> m a) -> m a

class Embed f t | t -> f where
  embed :: f t a -> t a
