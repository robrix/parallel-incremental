{-# LANGUAGE DeriveFunctor #-}
module Data.Relation
( Relation
, fromRelation
, fromPredicate
, singleton
, lookup
) where

import Control.Monad
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
