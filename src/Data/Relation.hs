{-# LANGUAGE DeriveFunctor #-}
module Data.Relation
( Relation
, fromRelation
) where

newtype Relation i a = Relation (i -> Maybe a)
  deriving (Functor)

fromRelation :: (i -> Maybe a) -> Relation i a
fromRelation = Relation
