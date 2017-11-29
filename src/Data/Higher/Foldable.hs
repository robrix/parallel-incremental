{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Foldable
( HFoldable(..)
, module F
) where

import Data.Higher.Function as F
import Data.Higher.Monoid as H

class HFoldable f where
  hfoldMap :: H.Monoid m => (a ~> m) -> f a ~> m
  hfold :: H.Monoid m => f m ~> m
  hfold = hfoldMap id
