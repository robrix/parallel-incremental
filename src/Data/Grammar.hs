{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module Data.Grammar
( Grammar(..)
) where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Recursive
import Text.Parser.Token

data Grammar n t a where
  Err :: [String] -> Grammar n t a
  Nul :: a -> Grammar n t a
  Sat :: (t -> Bool) -> Grammar n t t
  Alt :: Grammar n t a -> Grammar n t a -> Grammar n t a
  Seq :: (a -> b -> c) -> Grammar n t a -> Grammar n t b -> Grammar n t c
  Lab :: Grammar n t a -> String -> Grammar n t a
  End :: Grammar n t ()
  Var :: n a -> Grammar n t a
  Rec :: (n a -> Grammar n t a) -> Grammar n t a

instance Functor (Grammar n t) where
  fmap = liftA

instance Applicative (Grammar n t) where
  pure = Nul
  liftA2 = Seq

instance Alternative (Grammar n t) where
  empty = Err []
  (<|>) = Alt

instance Parsing (Grammar n t) where
  try = id
  (<?>) = Lab
  eof = End
  unexpected s = Err [s]
  notFollowedBy a = a *> empty <|> pure ()

instance RecursiveParsing (Grammar n t) where
  mu f = Rec (f . Var)

instance CharParsing (Grammar n Char) where
  satisfy = Sat

instance TokenParsing (Grammar n Char)
