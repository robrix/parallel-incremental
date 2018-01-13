module Data.DList where

import Data.Semigroup

newtype DList a = DList { unDList :: [a] -> [a] }

fromList :: [a] -> DList a
fromList as = DList (as ++)

instance Semigroup (DList a) where
  DList a <> DList b = DList (a . b)

instance Monoid (DList a) where
  mempty = DList id
  mappend = (<>)

instance Foldable DList where
  foldMap f (DList d) = foldMap f (d [])

instance Functor DList where
  fmap f (DList d) = DList (map f (d []) ++)

instance Applicative DList where
  pure a = DList (a:)

  DList f <*> DList a = DList ((f [] <*> a []) ++)
