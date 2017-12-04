module Data.Delta where

import Data.Function
import Data.Semigroup

data Delta
  = Delta
    { deltaLines   :: {-# UNPACK #-} !Int
    , deltaColumns :: {-# UNPACK #-} !Int
    , deltaBytes   :: {-# UNPACK #-} !Int
    }
  deriving (Show)

instance Eq Delta where
  (==) = (==) `on` deltaBytes

instance Ord Delta where
  compare = compare `on` deltaBytes

instance Semigroup Delta where
  Delta l1 c1 b1 <> Delta l2 c2 b2 = Delta (l1 + l2) (addColumns c1 c2) (b1 + b2)
    where addColumns c1 c2 | l1 == 0 || l2 == 0 = c1 + c2
                           | otherwise          = c2

instance Monoid Delta where
  mempty = Delta 0 0 0
  mappend = (<>)
