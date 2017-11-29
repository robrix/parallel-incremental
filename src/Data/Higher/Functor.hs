{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Functor
( Functor(..)
, module F
) where

import Data.Higher.Function as F
import Prelude hiding (Functor(..))

class Functor f where
  fmap :: (a ~> b) -> f a ~> f b
  (<$) :: (forall x . b x) -> f a ~> f b
  a <$ r = fmap (const a) r
