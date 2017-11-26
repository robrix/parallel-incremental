{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module Data.Grammar where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators

data Grammar n s a where
  Err :: [String] -> Grammar n s a
  Nul :: a -> Grammar n s a
  Sat :: (s -> Bool) -> Grammar n s s
  Alt :: Grammar n s a -> Grammar n s a -> Grammar n s a
  Seq :: (a -> b -> c) -> Grammar n s a -> Grammar n s b -> Grammar n s c
  Lab :: Grammar n s a -> String -> Grammar n s a
  End :: Grammar n s ()
  Var :: n a -> Grammar n s a
  Rec :: (forall n . n a -> Grammar n s a) -> Grammar n' s a

mu :: (forall n . Grammar n s a -> Grammar n s a) -> Grammar n' s a
mu f = Rec (f . Var)

instance Functor (Grammar n s) where
  fmap = liftA

instance Applicative (Grammar n s) where
  pure = Nul
  liftA2 = Seq

instance Alternative (Grammar n s) where
  empty = Err []
  (<|>) = Alt

instance Parsing (Grammar n s) where
  try = id
  (<?>) = Lab
  eof = End
  unexpected s = Err [s]
  notFollowedBy a = a *> empty <|> pure ()

instance CharParsing (Grammar n Char) where
  satisfy = Sat
