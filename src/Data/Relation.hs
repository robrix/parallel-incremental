{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module Data.Relation
( Relation
, fromRelation
, fromPredicate
, singleton
, lookup
, Semigroup(..)
, Semiring(..)
) where

import Control.Applicative
import Control.Monad
import Data.Semiring
import Prelude hiding (lookup)

newtype Relation i a = Relation (i -> Maybe a)
  deriving (Functor)

fromRelation :: (i -> Maybe a) -> Relation i a
fromRelation = Relation

fromPredicate :: (i -> Bool) -> Relation i i
fromPredicate predicate = Relation (\ i -> guard (predicate i) *> pure i)

singleton :: Eq i => i -> a -> Relation i a
singleton i a = Relation ((*> pure a) . guard . (== i))

lookup :: i -> Relation i a -> Maybe a
lookup i (Relation m) = m i


instance Semigroup (Relation i a) where
  Relation p1 <> Relation p2 = Relation ((<|>) <$> p1 <*> p2)

instance Monoid (Relation i a) where
  mempty = Relation (const Nothing)
  mappend = (<>)

instance Semigroup a => Semiring (Relation i a) where
  Relation p1 >< Relation p2 = Relation ((<>) <$> p1 <*> p2)

instance Semigroup i => Unital (Relation i i) where
  one = fromRelation Just
