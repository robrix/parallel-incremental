module Control.Higher.Applicative
( Applicative(..)
, module F
) where

import Data.Higher.Function as F
import Prelude hiding (Applicative(..))

class Applicative f where
