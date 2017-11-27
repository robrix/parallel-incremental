{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.Fix where

import Data.Function (fix)
import Data.Recursive

newtype Fix f a = Fix { unFix :: f (Fix f) a }

instance Recursive (Fix f) where
  mu = fix

instance Embed f (Fix f) where
  embed = Fix
