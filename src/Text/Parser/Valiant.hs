module Text.Parser.Valiant where

import Data.Bifunctor

data TwoNF t = U t | B (TwoNF t) (TwoNF t)

data Production t n = T t | N n

instance Bifunctor Production where
  bimap f _ (T t) = T (f t)
  bimap _ g (N n) = N (g n)
