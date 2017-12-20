{-# LANGUAGE TypeOperators #-}
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
