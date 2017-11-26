{-# LANGUAGE GADTs #-}
module Data.Grammar where

import Control.Applicative

data Grammar s n a where
  Empty :: Grammar s n a
  Nul :: a -> Grammar s n a
  Lit :: s -> Grammar s n s
  Alt :: Grammar s n a -> Grammar s n a -> Grammar s n a
  Seq :: (a -> b -> c) -> Grammar s n a -> Grammar s n b -> Grammar s n c

instance Functor (Grammar s n) where
  fmap = liftA

instance Applicative (Grammar s m) where
  pure = Nul
  liftA2 = Seq

instance Alternative (Grammar s n) where
  empty = Empty
  (<|>) = Alt
