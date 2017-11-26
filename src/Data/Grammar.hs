{-# LANGUAGE GADTs #-}
module Data.Grammar where

import Control.Applicative

data Grammar s a where
  Empty :: Grammar s a
  Nul :: a -> Grammar s a
  Lit :: s -> Grammar s s
  Alt :: Grammar s a -> Grammar s a -> Grammar s a
  Seq :: (a -> b -> c) -> Grammar s a -> Grammar s b -> Grammar s c

instance Functor (Grammar s) where
  fmap = liftA

instance Applicative (Grammar s) where
  pure = Nul
  liftA2 = Seq

instance Alternative (Grammar s) where
  empty = Empty
  (<|>) = Alt
