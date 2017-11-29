{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Functor
( HFunctor(..)
, module F
) where

import Data.Higher.Function as F

class HFunctor f where
  hfmap :: (a ~> b) -> f a ~> f b
