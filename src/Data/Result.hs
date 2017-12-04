{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Data.Result where

data Result e a
  = Failure [e]
  | Success a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
