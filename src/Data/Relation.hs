{-# LANGUAGE DeriveFunctor #-}
module Data.Relation
( Relation
) where

newtype Relation i a = Relation (i -> Maybe a)
  deriving (Functor)
