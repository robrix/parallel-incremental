{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Text.Parser.Valiant where

import Data.Bifunctor
import Data.Monoid

data BiNF s = U s | B (BiNF s) (BiNF s)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data CFG f t n = CFG { start :: n, rules :: [(n, f (Symbol t n))] }
  deriving (Foldable, Functor, Traversable)

size :: (Foldable f, Num a) => CFG f t n -> a
size = getSum . foldMap (fromIntegral . length . snd) . rules

data Symbol t n = T t | N n
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

symbol :: (t -> a) -> (n -> a) -> Symbol t n -> a
symbol f _ (T t) = f t
symbol _ g (N n) = g n

instance Bifunctor Symbol where
  bimap f g = symbol (T . f) (N . g)
