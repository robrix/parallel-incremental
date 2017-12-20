{-# LANGUAGE PolyKinds #-}
module Control.Higher.Comonad.Cofree where

import Data.Higher.Functor as H

data Cofree f a x = Cofree { extract :: a x, unwrap :: f (Cofree f a) x }

data CofreeF f a b x = CofreeF { headF :: a x, tailF :: f b x }

instance H.Functor f => H.Functor (CofreeF f a) where
  fmap f (CofreeF h t) = CofreeF h (H.fmap f t)
