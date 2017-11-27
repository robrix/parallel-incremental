module Text.Parser.Recursive
( RecursiveParsing(..)
) where

import Control.Applicative

class Alternative m => RecursiveParsing m where
  mu :: (m a -> m a) -> m a
