module Text.Parser.Valiant where

import Data.Bifunctor
import Data.Monoid

data BiNF s = U s | B (BiNF s) (BiNF s)

data CFG f t n = CFG { start :: n, rules :: [(n, f (Symbol t n))] }

size :: (Foldable f, Num a) => CFG f t n -> a
size = getSum . foldMap (fromIntegral . length . snd) . rules

data Symbol t n = T t | N n

symbol :: (t -> a) -> (n -> a) -> Symbol t n -> a
symbol f _ (T t) = f t
symbol _ g (N n) = g n

instance Bifunctor Symbol where
  bimap f g = symbol (T . f) (N . g)
