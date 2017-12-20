{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Traversable
( Traversable(..)
, module F
) where

import Control.Higher.Applicative as H
import Data.Higher.Foldable as H
import Data.Higher.Function as F
import Prelude hiding (Traversable(..))

class H.Foldable t => Traversable t where
  traverse :: H.Applicative f => (a ~> f b) -> t a ~> f (t b)
