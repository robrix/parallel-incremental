{-# LANGUAGE RankNTypes, TypeOperators #-}
module Control.Higher.Applicative
( Applicative(..)
, module F
) where

import Data.Higher.Functor as H
import Data.Higher.Function as F
import Prelude hiding (Applicative(..))

class H.Functor f => Applicative f where
  pure :: a ~> f a

  (<*>) :: f (a :-> b) x -> f a x -> f b x

  (<*) :: f a x -> f b x -> f a x
  (<*) = liftA2 const

  liftA2 :: (forall x. a x -> b x -> c x) -> f a x -> f b x -> f c x
