{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Fix where

import Data.Higher.Functor
import Data.Function (fix)
import Data.Recursive

newtype Fix f a = Fix { unFix :: f (Fix f) a }

hcata :: HFunctor f => (f a ~> a) -> Fix f ~> a
hcata alg = alg . hfmap (hcata alg) . unFix

instance Recursive (Fix f) where
  mu = fix

instance Embed Fix where
  embed = Fix
