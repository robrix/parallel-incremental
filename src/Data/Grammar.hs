{-# LANGUAGE GADTs #-}
module Data.Grammar where

import Control.Applicative

data Grammar n s a where
  Empty :: Grammar n s a
  Nul :: a -> Grammar n s a
  Lit :: s -> Grammar n s s
  Alt :: Grammar n s a -> Grammar n s a -> Grammar n s a
  Seq :: (a -> b -> c) -> Grammar n s a -> Grammar n s b -> Grammar n s c

sym :: [s] -> Grammar n s [s]
sym = traverse Lit

instance Functor (Grammar n s) where
  fmap = liftA

instance Applicative (Grammar n s) where
  pure = Nul
  liftA2 = Seq

instance Alternative (Grammar n s) where
  empty = Empty
  (<|>) = Alt
