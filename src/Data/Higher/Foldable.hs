{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Foldable
( HFoldable(..)
, module F
) where

import Data.Higher.Function as F
import Data.Higher.Monoid

class HFoldable f where
  hfoldMap :: HMonoid m => (a ~> m) -> f a ~> m
  hfold :: HMonoid m => f m ~> m
  hfold = hfoldMap id
