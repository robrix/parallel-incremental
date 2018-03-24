{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Data.Semiring
( Semiring(..)
, Semigroup(..)
, Mult(..)
) where

import Data.Semigroup
import GHC.Generics

class (Semigroup a, Monoid a) => Semiring a where
  zero :: a
  zero = mempty

  (><) :: a -> a -> a

  infixr 7 ><


newtype Mult a = Mult { getMult :: a }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

instance Semiring a => Semigroup (Mult a) where
  Mult a <> Mult b = Mult (a >< b)
