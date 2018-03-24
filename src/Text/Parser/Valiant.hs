module Text.Parser.Valiant where

import Data.Bifunctor
import Data.Monoid

data TwoNF t = U t | B (TwoNF t) (TwoNF t)

data CFG t n = CFG { start :: n, rules :: [(n, [Symbol t n])] }

size :: Num a => CFG t n -> a
size = getSum . foldMap (fromIntegral . length . snd) . rules

data Symbol t n = T t | N n

symbol :: (t -> a) -> (n -> a) -> Symbol t n -> a
symbol f _ (T t) = f t
symbol _ g (N n) = g n

instance Bifunctor Symbol where
  bimap f g = symbol (T . f) (N . g)
