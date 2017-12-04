{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Data.Recursive
( Recursive(..)
, Corecursive1(..)
, chainl1
) where

import Control.Applicative
import Data.Higher.Functor as H

class Recursive m where
  mu :: (m a -> m a) -> m a

class H.Functor (Cobase1 t) => Corecursive1 t where
  type Cobase1 t :: (* -> *) -> * -> *

  embed1 :: Cobase1 t t ~> t

  ana :: forall a . (a ~> Cobase1 t a) -> a ~> t
  ana coalgebra = go
    where go :: a ~> t
          go = embed1 . H.fmap go . coalgebra


chainl1 :: (Alternative m, Recursive m) => m a -> m (a -> a -> a) -> m a
chainl1 expr op = scan
  where scan = expr <**> rst
        rst = mu (\ more -> (\f y g x -> g (f x y)) <$> op <*> expr <*> more <|> pure id)
