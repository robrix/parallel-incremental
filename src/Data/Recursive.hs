{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeOperators #-}
module Data.Recursive
( Recursive(..)
, Base1
, Corecursive1(..)
, chainl1
) where

import Control.Applicative
import Data.Higher.Functor as H

class Recursive m where
  mu :: (m a -> m a) -> m a

type family Base1 (t :: * -> *) :: (* -> *) -> * -> *

class H.Functor (Base1 t) => Corecursive1 t where
  embed1 :: Base1 t t ~> t


chainl1 :: (Alternative m, Recursive m) => m a -> m (a -> a -> a) -> m a
chainl1 expr op = scan
  where scan = expr <**> rst
        rst = mu (\ more -> (\f y g x -> g (f x y)) <$> op <*> expr <*> more <|> pure id)
