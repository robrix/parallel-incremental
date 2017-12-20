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

  (<*>) :: f (a :-> b) x -> f a x -> f b x
  (<*>) = liftA2 unA

  infixl 4 <*>

  (<*) :: f a x -> f b x -> f a x
  (<*) = liftA2 const

  infixl 4 <*

  (*>) :: f a x -> f b x -> f b x
  (*>) = liftA2 (const id)

  infixl 4 *>

  liftA2 :: (forall x. a x -> b x -> c x) -> f a x -> f b x -> f c x
  liftA2 f x = (H.fmap (A . f) x <*>)

  {-# MINIMAL pure, ((<*>) | liftA2) #-}
