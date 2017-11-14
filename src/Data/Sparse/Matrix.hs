{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Data.Sparse.Matrix where

import Data.Sparse.Shape
import qualified Data.Sparse.Vector as V

data M s1 s2 a where
  Z :: M x y a
  O :: a -> M 'L 'L a
  R :: M x1 'L a -> M x2 'L a -> M ('B x1 x2) 'L a
  C :: M 'L y1 a -> M 'L y2 a -> M 'L ('B y1 y2) a
  Q :: M x1 y1 a -> M x2 y1 a
    -> M x1 y2 a -> M x2 y2 a
    -> M ('B x1 x2) ('B y1 y2) a

deriving instance Eq a   => Eq   (M s1 s2 a)
deriving instance Show a => Show (M s1 s2 a)
