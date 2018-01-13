module Data.DList where

import Data.Semigroup

newtype DList a = DList { unDList :: [a] -> [a] }

char :: Char -> DList Char
char c = DList (c:)

toS :: DList Char -> String
toS = ($ "") . unDList

instance Semigroup (DList a) where
  DList a <> DList b = DList (a . b)

instance Monoid (DList a) where
  mempty = DList id
  mappend = (<>)