{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Traversable
( Traversable(..)
, module F
) where

import Control.Higher.Applicative as H
import Data.Higher.Foldable as H
import Data.Higher.Function as F
import Data.Higher.Functor as H
import Prelude hiding (Traversable(..))

class (H.Foldable t, H.Functor t) => Traversable t where
  traverse :: H.Applicative f => (a ~> f b) -> t a ~> f (t b)

  sequenceA :: H.Applicative f => t (f a) ~> f (t a)
