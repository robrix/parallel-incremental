{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module Data.Grammar
( Grammar(..)
, Rec(..)
) where

import Control.Applicative
import Data.Rec
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Recursive
import Text.Parser.Token

data Grammar t r a where
  Err :: [String] -> Grammar t r a
  Nul :: a -> Grammar t r a
  Sat :: (t -> Bool) -> Grammar t r t
  Alt :: r a -> r a -> Grammar t r a
  Seq :: (a -> b -> c) -> r a -> r b -> Grammar t r c
  Lab :: r a -> String -> Grammar t r a
  End :: Grammar t r ()

instance Functor (Rec n (Grammar t)) where
  fmap = liftA

instance Applicative (Rec n (Grammar t)) where
  pure = In . Nul
  liftA2 f a b = In (Seq f a b)

instance Alternative (Rec n (Grammar t)) where
  empty = In (Err [])
  a <|> b = In (Alt a b)

instance Parsing (Rec n (Grammar t)) where
  try = id
  a <?> s = In (Lab a s)
  eof = In End
  unexpected s = In (Err [s])
  notFollowedBy a = a *> empty <|> pure ()

instance RecursiveParsing (Rec n (Grammar t)) where
  mu f = Rec (f . Var)

instance CharParsing (Rec n (Grammar Char)) where
  satisfy = In . Sat

instance TokenParsing (Rec n (Grammar Char))
