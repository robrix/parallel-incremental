{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, RankNTypes, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Grammar
( Grammar(..)
) where

import Control.Applicative
import Control.Monad (guard)
import qualified Data.Higher.Foldable as H
import qualified Data.Higher.Functor as H
import qualified Data.Higher.Monoid as H
import Data.Higher.Functor.Classes as H
import Data.Recursive
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

data Grammar t r a
  = Err [String]
  | Nul a
  | Sat (t -> Maybe a)
  | Alt (r a) (r a)
  | forall c b . Seq (c -> b -> a) (r c) (r b)
  | Lab (r a) String
  | End a

instance (Bounded t, Enum t, Show t) => H.Show1 (Grammar t) where
  liftShowsPrec sp d g = case g of
    Err es    -> showsUnaryWith   showsPrec              "Err" d es
    Nul a     -> showsUnaryWith   hide                   "Nul" d a
    Sat p     -> showsUnaryWith   hide                   "Sat" d p
    Alt a b   -> showsBinaryWith  sp        sp           "Alt" d a b
    Seq f a b -> showsTernaryWith hide      sp        sp "Seq" d f a b
    Lab r s   -> showsBinaryWith  sp        showsPrec    "Lab" d r s
    End a     -> showsUnaryWith   hide                   "End" d a
    where hide _ _ = showChar '_'

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t) => Functor (r (Grammar t)) where
  fmap = liftA

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t) => Applicative (r (Grammar t)) where
  pure = embed1 . Nul
  liftA2 f a b = embed1 (Seq f a b)

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t, Mu1 (r (Grammar t))) => Alternative (r (Grammar t)) where
  empty = embed1 (Err [])
  a <|> b = embed1 (Alt a b)
  many a = mu1 (\ more -> (:) <$> a <*> more <|> pure [])
  some a = (:) <$> a <*> many a

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t, Mu1 (r (Grammar t))) => Parsing (r (Grammar t)) where
  try = id
  a <?> s = embed1 (Lab a s)
  eof = embed1 (End ())
  unexpected s = embed1 (Err [s])
  notFollowedBy a = a *> empty <|> pure ()

instance (Corecursive1 (r (Grammar Char)), Cobase1 (r (Grammar Char)) ~ Grammar Char, Mu1 (r (Grammar Char))) => CharParsing (r (Grammar Char)) where
  satisfy p = embed1 (Sat (\ c -> guard (p c) *> Just c))

instance (Corecursive1 (r (Grammar Char)), Cobase1 (r (Grammar Char)) ~ Grammar Char, Mu1 (r (Grammar Char))) => TokenParsing (r (Grammar Char))

instance H.Foldable (Grammar t) where
  foldMap f g = case g of
    Err _     -> H.mempty
    Nul _     -> H.mempty
    Sat _     -> H.mempty
    Alt a b   -> f a H.<> f b
    Seq _ a b -> f a H.<> f b
    Lab r _   -> f r
    End _     -> H.mempty

instance H.Functor (Grammar t) where
  fmap f g = case g of
    Err es    -> Err es
    Nul a     -> Nul a
    Sat p     -> Sat p
    Alt a b   -> Alt (f a) (f b)
    Seq g a b -> Seq g (f a) (f b)
    Lab r s   -> Lab (f r) s
    End a     -> End a
