module Data.Higher.Traversable
( Traversable(..)
, module F
) where

import Data.Higher.Foldable as H
import Data.Higher.Function as F
import Prelude hiding (Traversable(..))

class H.Foldable f => Traversable f where
