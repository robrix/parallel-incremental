{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Data.Result where

import Control.Applicative
import Data.Align
import Data.Bifunctor
import Data.These

data Result e a
  = Failure [e]
  | Success a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifunctor Result where
  bimap f _ (Failure e) = Failure (f <$> e)
  bimap _ g (Success a) = Success (g a)

instance Applicative (Result e) where
  pure = Success

  Success f  <*> a          = f <$> a
  Failure e1 <*> Failure e2 = Failure (e1 ++ e2)
  Failure e  <*> _          = Failure e

instance Alternative (Result e) where
  empty = Failure []

  Failure e1 <|> Failure e2 = Failure (e1 ++ e2)
  Success a  <|> _          = Success a
  _          <|> Success b  = Success b

instance Align (Result e) where
  nil = empty

  alignWith f (Success a) (Success b) = Success (f (These a b))
  alignWith f (Success a) _           = Success (f (This  a))
  alignWith f _           (Success b) = Success (f (That    b))
  alignWith _ (Failure a) (Failure b) = Failure (a ++ b)
