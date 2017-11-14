{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}
module Data.Sparse.Vector where

import qualified Data.Sparse.Shape as S

data V s a where
  Z :: V s a
  O :: a -> V 'S.L a
  B :: V s1 a -> V s2 a -> V ('S.B s1 s2) a

deriving instance Eq a   => Eq   (V s a)
deriving instance Show a => Show (V s a)


instance Foldable (V s) where
  foldMap _ Z = mempty
  foldMap f (O a) = f a
  foldMap f (B v1 v2) = foldMap f v1 `mappend` foldMap f v2

instance Functor (V s) where
  fmap _ Z = Z
  fmap f (O a) = O (f a)
  fmap f (B v1 v2) = B (fmap f v1) (fmap f v2)
