{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Data.Result where

data Result e a
  = Failure [e]
  | Success a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative (Result e) where
  pure = Success

  Success f  <*> a          = f <$> a
  Failure e1 <*> Failure e2 = Failure (e1 ++ e2)
  Failure e  <*> _          = Failure e
