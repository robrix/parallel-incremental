{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes #-}
module Data.Grammar
( Grammar(..)
) where

import Control.Applicative
import Data.Recursive
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

data Grammar t r a where
  Err :: [String] -> Grammar t r a
  Nul :: a -> Grammar t r a
  Sat :: (t -> Bool) -> Grammar t r t
  Alt :: r a -> r a -> Grammar t r a
  Seq :: (a -> b -> c) -> r a -> r b -> Grammar t r c
  Lab :: r a -> String -> Grammar t r a
  End :: Grammar t r ()

instance Embed r => Functor (r (Grammar t)) where
  fmap = liftA

instance Embed r => Applicative (r (Grammar t)) where
  pure = embed . Nul
  liftA2 f a b = embed (Seq f a b)

instance Embed r => Alternative (r (Grammar t)) where
  empty = embed (Err [])
  a <|> b = embed (Alt a b)

instance Embed r => Parsing (r (Grammar t)) where
  try = id
  a <?> s = embed (Lab a s)
  eof = embed End
  unexpected s = embed (Err [s])
  notFollowedBy a = a *> empty <|> pure ()

instance Embed r => CharParsing (r (Grammar Char)) where
  satisfy = embed . Sat

instance Embed r => TokenParsing (r (Grammar Char))
