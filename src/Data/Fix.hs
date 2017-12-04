{-# LANGUAGE RankNTypes, TypeFamilies, TypeOperators #-}
module Data.Fix where

import Data.Higher.Functor as H
import Data.Function (fix)
import Data.Recursive

newtype Fix f a = Fix { unFix :: f (Fix f) a }

instance Mu1 (Fix f) where
  mu1 = fix

instance H.Functor f => Recursive1 (Fix f) where
  type Base1 (Fix f) = f
  project1 = unFix

instance H.Functor f => Corecursive1 (Fix f) where
  type Cobase1 (Fix f) = f
  embed1 = Fix
