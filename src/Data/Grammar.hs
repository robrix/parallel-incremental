{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes, UndecidableInstances #-}
module Data.Grammar
( Grammar(..)
) where

import Control.Applicative
import qualified Data.Higher.Foldable as H
import qualified Data.Higher.Functor as H
import qualified Data.Higher.Monoid as H
import Data.Higher.Functor.Classes as H
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

instance (Bounded t, Enum t, Show t) => H.Show1 (Grammar t) where
  liftShowsPrec sp d g = case g of
    Err es    -> showsUnaryWith   showsPrec              "Err" d es
    Nul a     -> showsUnaryWith   hide                   "Nul" d a
    Sat p     -> showsUnaryWith   hide                   "Sat" d p
    Alt a b   -> showsBinaryWith  sp        sp           "Alt" d a b
    Seq f a b -> showsTernaryWith hide      sp        sp "Seq" d f a b
    Lab r s   -> showsBinaryWith  sp        showsPrec    "Lab" d r s
    End       -> showString "End"
    where hide _ _ = showChar '_'

instance Embed r => Functor (r (Grammar t)) where
  fmap = liftA

instance Embed r => Applicative (r (Grammar t)) where
  pure = embed . Nul
  liftA2 f a b = embed (Seq f a b)

instance (Embed r, Recursive (r (Grammar t))) => Alternative (r (Grammar t)) where
  empty = embed (Err [])
  a <|> b = embed (Alt a b)
  many a = mu (\ more -> (:) <$> a <*> more <|> pure [])
  some a = (:) <$> a <*> many a

instance (Embed r, Recursive (r (Grammar t))) => Parsing (r (Grammar t)) where
  try = id
  a <?> s = embed (Lab a s)
  eof = embed End
  unexpected s = embed (Err [s])
  notFollowedBy a = a *> empty <|> pure ()

instance (Embed r, Recursive (r (Grammar Char))) => CharParsing (r (Grammar Char)) where
  satisfy = embed . Sat

instance (Embed r, Recursive (r (Grammar Char))) => TokenParsing (r (Grammar Char))

instance H.Foldable (Grammar t) where
  foldMap f g = case g of
    Err _     -> H.mempty
    Nul _     -> H.mempty
    Sat _     -> H.mempty
    Alt a b   -> f a H.<> f b
    Seq _ a b -> f a H.<> f b
    Lab r _   -> f r
    End       -> H.mempty

instance H.Functor (Grammar t) where
  fmap f g = case g of
    Err es    -> Err es
    Nul a     -> Nul a
    Sat p     -> Sat p
    Alt a b   -> Alt (f a) (f b)
    Seq g a b -> Seq g (f a) (f b)
    Lab r s   -> Lab (f r) s
    End       -> End
