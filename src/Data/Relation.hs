{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Data.Relation
( Relation
, fromList
, fromRelation
, fromPredicate
, singleton
, lookup
, related
, Semigroup(..)
, Semiring(..)
) where

import Control.Applicative
import qualified Control.Category as Cat
import Control.Monad
import qualified Data.Map as Map
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Semiring
import Prelude hiding (lookup)

newtype Relation f i a = Relation { runRelation :: i -> f a }
  deriving (Functor)

fromList :: Ord i => [(i, a)] -> Relation Maybe i a
fromList [] = mempty
fromList [(i, a)] = singleton i a
fromList list = Relation (`Map.lookup` map)
  where map = Map.fromList list

fromRelation :: (i -> f a) -> Relation f i a
fromRelation = Relation

fromPredicate :: Alternative f => (i -> Bool) -> Relation f i i
fromPredicate predicate = Relation (\ i -> guard (predicate i) *> pure i)

singleton :: (Alternative f, Eq i) => i -> a -> Relation f i a
singleton i a = Relation ((*> pure a) . guard . (== i))

lookup :: i -> Relation f i a -> f a
lookup i (Relation m) = m i

related :: (Foldable f, Eq a) => Relation f i a -> i -> a -> Bool
related r = flip elem . flip lookup r


instance Alternative f => Semigroup (Relation f i a) where
  Relation p1 <> Relation p2 = Relation ((<|>) <$> p1 <*> p2)

instance Alternative f => Monoid (Relation f i a) where
  mempty = Relation (const empty)
  mappend = (<>)

instance (Alternative f, Semigroup a) => Semiring (Relation f i a) where
  Relation p1 >< Relation p2 = Relation (liftA2 (<>) <$> p1 <*> p2)

instance (Alternative f, Semigroup i) => Unital (Relation f i i) where
  one = fromRelation pure

instance (Alternative m, Monad m) => Cat.Category (Relation m) where
  id = Relation pure
  r2 . r1 = Relation (\ i -> lookup i r1 >>= flip lookup r2)

instance Functor m => Profunctor (Relation m) where
  dimap f g (Relation r) = Relation (fmap g . r . f)

instance Functor m => Strong (Relation m) where
  first'  (Relation r) = Relation (\ (i, j) -> flip (,) j <$> r i)
  second' (Relation r) = Relation (\ (i, j) ->      (,) i <$> r j)

instance Applicative m => Choice (Relation m) where
  left'  (Relation r) = Relation (either (fmap   Left . r) (pure . Right))
  right' (Relation r) = Relation (either (pure . Left)     (fmap   Right . r))

instance Functor m => Sieve (Relation m) m where
  sieve = runRelation

instance Functor m => Representable (Relation m) where
  type Rep (Relation m) = m
  tabulate = Relation
