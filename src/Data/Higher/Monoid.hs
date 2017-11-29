module Data.Higher.Monoid
( Monoid(..)
, (<>)
, Lift(..)
) where

import Data.Higher.Semigroup (Semigroup(..))
import qualified Data.Monoid as M
import Prelude hiding (Monoid)

class Semigroup m => Monoid m where
  mempty :: m a

newtype Lift m a = Lift { lower :: m }

instance M.Monoid m => Semigroup (Lift m) where
  Lift a <> Lift b = Lift (a `M.mappend` b)

instance M.Monoid m => Monoid (Lift m) where
  mempty = Lift M.mempty
