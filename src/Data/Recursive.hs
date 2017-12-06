{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Recursive
( Mu1(..)
, Recursive1(..)
, Corecursive1(..)
, chainl1
) where

import Control.Applicative
import Data.Higher.Functor as H
import GHC.Generics

class Mu1 t where
  mu1 :: (t a -> t a) -> t a

class H.Functor (Base1 t) => Recursive1 t where
  type Base1 t :: (* -> *) -> * -> *

  project1 :: t ~> Base1 t t

  cata :: forall a . (Base1 t a ~> a) -> t ~> a
  cata algebra = go
    where go :: t ~> a
          go = algebra . H.fmap go . project1

  para :: forall a . (Base1 t (t :*: a) ~> a) -> t ~> a
  para algebra = go
    where go :: t ~> a
          go = algebra . H.fmap (id &&& go) . project1

(&&&) :: (a -> l b) -> (a -> r b) -> a -> (l :*: r) b
f &&& g = \ a -> f a :*: g a

infixr 3 &&&

class H.Functor (Cobase1 t) => Corecursive1 t where
  type Cobase1 t :: (* -> *) -> * -> *

  embed1 :: Cobase1 t t ~> t

  ana :: forall a . (a ~> Cobase1 t a) -> a ~> t
  ana coalgebra = go
    where go :: a ~> t
          go = embed1 . H.fmap go . coalgebra

  apo :: forall a . (a ~> Cobase1 t (t :+: a)) -> a ~> t
  apo coalgebra = go
    where go :: a ~> t
          go = embed1 . H.fmap (id ||| go) . coalgebra

(|||) :: (l a -> b) -> (r a -> b) -> (l :+: r) a -> b
f ||| g = \ s -> case s of L1 l -> f l
                           R1 r -> g r

infixr 2 |||

chainl1 :: (Alternative m, Mu1 m) => m a -> m (a -> a -> a) -> m a
chainl1 expr op = scan
  where scan = expr <**> rst
        rst = mu1 (\ more -> (\f y g x -> g (f x y)) <$> op <*> expr <*> more <|> pure id)
