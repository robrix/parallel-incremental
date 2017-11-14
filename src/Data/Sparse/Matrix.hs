{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, StandaloneDeriving #-}
module Data.Sparse.Matrix where

import Data.Semigroup
import Data.Semiring
import Data.Sparse.Shape
import qualified Data.Sparse.Vector as V

data M x y a where
  Z :: M x y a
  O :: a -> M 'L 'L a
  R :: M x1 'L a -> M x2 'L a -> M ('B x1 x2) 'L a
  C :: M 'L y1 a -> M 'L y2 a -> M 'L ('B y1 y2) a
  Q :: M x1 y1 a -> M x2 y1 a
    -> M x1 y2 a -> M x2 y2 a
    -> M ('B x1 x2) ('B y1 y2) a

deriving instance Eq a   => Eq   (M x y a)
deriving instance Show a => Show (M x y a)

data SomeM a where
  SomeM :: M x y a -> SomeM a

deriving instance Show a => Show (SomeM a)

z :: M x y a
z = Z

o :: (Eq a, Monoid a) => a -> M 'L 'L a
o x | x == zero = Z
    | otherwise = O x

q0 :: M ('B x1 x2) ('B y1 y2) a
q0 = Q Z Z Z Z

q :: M x1 y1 a -> M x2 y1 a -> M x1 y2 a -> M x2 y2 a -> M ('B x1 x2) ('B y1 y2) a
q Z Z Z Z = Z
q a11 a12 a21 a22 = Q a11 a12 a21 a22

r :: M x1 'L a -> M x2 'L a -> M ('B x1 x2) 'L a
r Z Z = Z
r a11 a12 = R a11 a12

c :: M 'L y1 a -> M 'L y2 a -> M 'L ('B y1 y2) a
c Z Z = Z
c a11 a21 = C a11 a21


fromVec :: V.V s a -> M s s a
fromVec V.Z = Z
fromVec (V.L a) = O a
fromVec (V.B v1 v2) = q (fromVec v1) Z Z (fromVec v2)


instance (Eq a, Monoid a, Semigroup a) => Semigroup (M x y a) where
  Z <> x = x
  x <> Z = x
  O x <> O y = o (x <> y)
  R a11 a12 <> R b11 b12 = r (a11 <> b11) (a12 <> b12)
  C a11 a21 <> C b11 b21 = c (a11 <> b11) (a21 <> b21)
  Q a11 a12 a21 a22 <> Q b11 b12 b21 b22
    = q (a11 <> b11) (a12 <> b12)
        (a21 <> b21) (a22 <> b22)

instance (Eq a, Monoid a, Semigroup a) => Monoid (M x y a) where
  mempty = Z
  mappend = (<>)

infixr 7 `mult`

mult :: (Eq a, Semiring a) => M x y a -> M z x a -> M z y a
Z `mult` _ = Z
_ `mult` Z = Z
O x `mult` O y = o (x >< y)
O x `mult` R a11 a12 = r (O x `mult` a11) (O x `mult` a12)
C a11 a21 `mult` O x = c (a11 `mult` O x) (a21 `mult` O x)
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
v _ Z _ = Z
v Z x Z = x
v (Q a11 a12 _ a22) (Q x11 x12 x21 x22) (Q b11 b12 _ b22)
  = q y11 y12
      y21 y22
  where y11 = v a11 (x11 <> a12 `mult` y21)                   b11
        y12 = v a11 (x12 <> a12 `mult` y22 <> y11 `mult` b12) b22
        y21 = v a22  x21                                      b11
        y22 = v a22 (x22 <>                   y21 `mult` b12) b22
v Z     x@Q{} b@Q{} = v q0 x b
v a@Q{} x@Q{} Z     = v a  x q0
v (Q a11 a12 _ a22) (C x11 x21) Z = c y11 y21
  where y21 = v a22 x21 Z
        y11 = v a11 (a12 `mult` y21 <> x11) Z
v Z (R x11 x12) (Q b11 b12 _ b22) = r y11 y12
  where y11 = v Z x11 b11
        y12 = v Z (y11 `mult` b12 <> x12) b22
