{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Data.Sparse.Matrix where

import Data.Semigroup
import Data.Semiring
import qualified Data.Sparse.Shape as S
import qualified Data.Sparse.Vector as V

data M x y a where
  N :: M x y a
  U :: a -> M 'S.U 'S.U a
  R :: M x1 'S.U a -> M x2 'S.U a -> M ('S.B x1 x2) 'S.U a
  C :: M 'S.U y1 a
    -> M 'S.U y2 a
    -> M 'S.U ('S.B y1 y2) a
  Q :: M x1 y1 a -> M x2 y1 a
    -> M x1 y2 a -> M x2 y2 a
    -> M ('S.B x1 x2) ('S.B y1 y2) a

deriving instance Eq a   => Eq   (M x y a)
deriving instance Show a => Show (M x y a)

data SomeM a where
  SomeM :: M x y a -> SomeM a

deriving instance Show a => Show (SomeM a)

z :: M x y a
z = N

l :: (Eq a, Monoid a) => a -> M 'S.U 'S.U a
l x | x == mempty = N
    | otherwise   = U x

q0 :: M ('S.B x1 x2) ('S.B y1 y2) a
q0 = Q N N N N

q :: M x1 y1 a -> M x2 y1 a -> M x1 y2 a -> M x2 y2 a -> M ('S.B x1 x2) ('S.B y1 y2) a
q N N N N = N
q a11 a12 a21 a22 = Q a11 a12 a21 a22

r :: M x1 'S.U a -> M x2 'S.U a -> M ('S.B x1 x2) 'S.U a
r N N = N
r a11 a12 = R a11 a12

c :: M 'S.U y1 a -> M 'S.U y2 a -> M 'S.U ('S.B y1 y2) a
c N N = N
c a11 a21 = C a11 a21


fromVec :: V.V s a -> M s s a
fromVec V.N = N
fromVec (V.U a) = U a
fromVec (V.B v1 v2) = q (fromVec v1) N N (fromVec v2)


instance (Eq a, Monoid a, Semigroup a) => Semigroup (M x y a) where
  N <> x = x
  x <> N = x
  U x <> U y = l (x <> y)
  R a11 a12 <> R b11 b12 = r (a11 <> b11) (a12 <> b12)
  C a11 a21 <> C b11 b21 = c (a11 <> b11) (a21 <> b21)
  Q a11 a12 a21 a22 <> Q b11 b12 b21 b22
    = q (a11 <> b11) (a12 <> b12)
        (a21 <> b21) (a22 <> b22)

instance (Eq a, Monoid a, Semigroup a) => Monoid (M x y a) where
  mempty = N
  mappend = (<>)

infixr 7 `mult`

mult :: (Eq a, Semiring a) => M x y a -> M z x a -> M z y a
N `mult` _ = N
_ `mult` N = N
U x `mult` U y = l (x >< y)
U x `mult` R a11 a12 = r (U x `mult` a11) (U x `mult` a12)
C a11 a21 `mult` U x = c (a11 `mult` U x) (a21 `mult` U x)
R a11 a12 `mult` C b11 b21 = a11 `mult` b11 <> a12 `mult` b21
C a11 a21 `mult` R b11 b12
  = q (a11 `mult` b11) (a11 `mult` b12)
      (a21 `mult` b11) (a21 `mult` b12)
R a11 a12 `mult` Q b11 b12 b21 b22 = r (a11 `mult` b11 <> a12 `mult` b21) (a11 `mult` b12 <> a12 `mult` b22)
Q a11 a12 a21 a22 `mult` C b11 b21 = c (a11 `mult` b11 <> a12 `mult` b21) (a21 `mult` b11 <> a22 `mult` b21)
Q a11 a12 a21 a22 `mult` Q b11 b12 b21 b22
  = q (a11 `mult` b11 <> a12 `mult` b21) (a11 `mult` b12 <> a12 `mult` b22)
      (a21 `mult` b11 <> a22 `mult` b21) (a21 `mult` b12 <> a22 `mult` b22)

instance (Eq a, Semiring a) => Semiring (M z z a) where
  (><) = mult


v :: (Eq a, Semiring a) => M x x a -> M y x a -> M y y a -> M y x a
v _ N _ = N
v N x N = x
v (Q a11 a12 _ a22) (Q x11 x12 x21 x22) (Q b11 b12 _ b22)
  = q y11 y12
      y21 y22
  where y11 = v a11 (x11 <> a12 `mult` y21)                   b11
        y12 = v a11 (x12 <> a12 `mult` y22 <> y11 `mult` b12) b22
        y21 = v a22  x21                                      b11
        y22 = v a22 (x22 <>                   y21 `mult` b12) b22
v N     x@Q{} b@Q{} = v q0 x b
v a@Q{} x@Q{} N     = v a  x q0
v (Q a11 a12 _ a22) (C x11 x21) N = c y11 y21
  where y21 = v a22 x21 N
        y11 = v a11 (a12 `mult` y21 <> x11) N
v N (R x11 x12) (Q b11 b12 _ b22) = r y11 y12
  where y11 = v N x11 b11
        y12 = v N (y11 `mult` b12 <> x12) b22
