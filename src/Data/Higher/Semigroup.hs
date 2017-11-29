module Data.Higher.Semigroup where

import qualified Data.Semigroup as S

class Semigroup s where
  (<>) :: s a -> s a -> s a


newtype Lift m a = Lift { lower :: m }

instance S.Semigroup s => Semigroup (Lift s) where
  Lift a <> Lift b = Lift (a S.<> b)
