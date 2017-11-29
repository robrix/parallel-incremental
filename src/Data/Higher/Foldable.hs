{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Foldable
( Foldable(..)
, module F
) where

import Data.Higher.Function as F
import Data.Higher.Monoid as H
import Prelude hiding (Foldable(..))

class Foldable f where
  foldMap :: H.Monoid m => (a ~> m) -> f a ~> m
  fold :: H.Monoid m => f m ~> m
  fold = foldMap id
