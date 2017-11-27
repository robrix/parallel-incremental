module Data.Recursive
( Recursive(..)
) where

import Control.Applicative

class Recursive m where
  mu :: (m a -> m a) -> m a
