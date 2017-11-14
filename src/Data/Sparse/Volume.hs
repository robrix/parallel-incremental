{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}
module Data.Sparse.Volume where

import qualified Data.Sparse.Shape as S

data V x y z a where
  Z :: V x y z a
  L :: a -> V 'S.L 'S.L 'S.L a
  R :: V x1 'S.L z1 a -> V x2 'S.L z2 a -> V ('S.B x1 x2) 'S.L ('S.B z1 z2) a
  C :: V 'S.L y1 z1 a -> V 'S.L y2 z2 a -> V 'S.L ('S.B y1 y2) ('S.B z1 z2) a
  O :: V x1 y1 z1 a -> V x2 y1 z1 a
    -> V x1 y2 z1 a -> V x2 y2 z1 a
    -> V x1 y1 z2 a -> V x2 y1 z2 a
    -> V x1 y2 z2 a -> V x2 y2 z2 a
    -> V ('S.B x1 x2) ('S.B y1 y2) ('S.B z1 z2) a

deriving instance Eq a   => Eq   (V x y z a)
deriving instance Show a => Show (V x y z a)
