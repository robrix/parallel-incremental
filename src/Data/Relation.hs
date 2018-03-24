{-# LANGUAGE DeriveFunctor #-}
module Data.Relation
( Relation
, fromRelation
, fromPredicate
) where

import Control.Monad

newtype Relation i a = Relation (i -> Maybe a)
  deriving (Functor)

fromRelation :: (i -> Maybe a) -> Relation i a
fromRelation = Relation

fromPredicate :: (i -> Bool) -> Relation i i
fromPredicate predicate = Relation (\ i -> guard (predicate i) *> pure i)
