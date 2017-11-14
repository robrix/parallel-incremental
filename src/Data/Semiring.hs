module Data.Semiring where

import Data.Semigroup

zero :: Monoid a => a
zero = mempty

class (Semigroup a, Monoid a) => Semiring a where
  (><) :: a -> a -> a

  infixr 7 ><
