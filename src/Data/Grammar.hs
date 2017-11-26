{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Grammar where

import Control.Applicative

data Grammar n s a where
  Empty :: Grammar n s a
  Nul :: a -> Grammar n s a
  Lit :: s -> Grammar n s s
  Alt :: Grammar n s a -> Grammar n s a -> Grammar n s a
  Seq :: (a -> b -> c) -> Grammar n s a -> Grammar n s b -> Grammar n s c
  Lab :: Grammar n s a -> String -> Grammar n s a
  Var :: n a -> Grammar n s a
  Rec :: (forall n . n a -> Grammar n s a) -> Grammar n' s a

sym :: [s] -> Grammar n s [s]
sym = traverse Lit

(<?>) :: Grammar n s a -> String -> Grammar n s a
(<?>) = Lab

infixr 0 <?>

mu :: (forall n . Grammar n s a -> Grammar n s a) -> Grammar n' s a
mu f = Rec (f . Var)

instance Functor (Grammar n s) where
  fmap = liftA

instance Applicative (Grammar n s) where
  pure = Nul
  liftA2 = Seq

instance Alternative (Grammar n s) where
  empty = Empty
  (<|>) = Alt
