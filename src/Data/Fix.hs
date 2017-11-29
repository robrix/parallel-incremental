{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Fix where

import Data.Higher.Functor as H
import Data.Function (fix)
import Data.Recursive

newtype Fix f a = Fix { unFix :: f (Fix f) a }

hcata :: H.Functor f => (f a ~> a) -> Fix f ~> a
hcata alg = alg . H.fmap (hcata alg) . unFix

instance Recursive (Fix f) where
  mu = fix

instance Embed Fix where
  embed = Fix
