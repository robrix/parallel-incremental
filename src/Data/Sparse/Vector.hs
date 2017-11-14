{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}
module Data.Sparse.Vector where

import qualified Data.Sparse.Shape as S

data V s a where
  Z :: V s a
  O :: a -> V 'S.L a
  B :: V s1 a -> V s2 a -> V ('S.B s1 s2) a

deriving instance Eq a   => Eq   (V s a)
deriving instance Show a => Show (V s a)
