module Data.Semiring
( Semiring(..)
, Semigroup(..)
) where

import Data.Semigroup

class (Semigroup a, Monoid a) => Semiring a where
  zero :: a
  zero = mempty

  (><) :: a -> a -> a

  infixr 7 ><
