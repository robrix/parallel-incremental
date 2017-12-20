{-# LANGUAGE RankNTypes, TypeOperators #-}
module Control.Higher.Applicative
( Applicative(..)
, module F
) where

import Data.Higher.Function as F
import Data.Higher.Functor as H
import Prelude hiding (Applicative(..))

class H.Functor f => Applicative f where
  pure :: a ~> f a

  liftA2 :: (a x -> b y -> c z) -> f a x -> f b y -> f c z

  {-# MINIMAL pure, liftA2 #-}
