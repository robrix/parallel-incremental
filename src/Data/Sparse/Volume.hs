{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}
module Data.Sparse.Volume where

import qualified Data.Sparse.Shape as S

data V x y z a where
  N :: V x y z a
  U :: a -> V 'S.U 'S.U 'S.U a
  R :: V x1 'S.U z1 a -> V x2 'S.U z2 a -> V ('S.B x1 x2) 'S.U ('S.B z1 z2) a
  C :: V 'S.U y1 z1 a -> V 'S.U y2 z2 a -> V 'S.U ('S.B y1 y2) ('S.B z1 z2) a
  O :: V x1 y1 z1 a -> V x2 y1 z1 a
    -> V x1 y2 z1 a -> V x2 y2 z1 a
    -> V x1 y1 z2 a -> V x2 y1 z2 a
    -> V x1 y2 z2 a -> V x2 y2 z2 a
    -> V ('S.B x1 x2) ('S.B y1 y2) ('S.B z1 z2) a

deriving instance Eq a   => Eq   (V x y z a)
deriving instance Show a => Show (V x y z a)
