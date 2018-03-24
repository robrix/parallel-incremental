{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Text.Parser.Valiant where

import Data.Bifunctor
import Data.Functor.Classes
import Data.Monoid hiding ((<>))
import Data.Semigroup

data CFG f t n = CFG { start :: n, rules :: [(n, f (Symbol t n))] }
  deriving (Foldable, Functor, Traversable)

size :: (Foldable f, Num a) => CFG f t n -> a
size = getSum . foldMap (fromIntegral . length . snd) . rules

instance (Show1 f, Show t, Show n) => Show (CFG f t n) where
  showsPrec d (CFG s r) = showParen (d > 10) $ showString "CFG { start = " . showsPrec 0 s . showString ", rules" . liftShowsPrec (liftShowsPrec showsPrec1 showList1) (liftShowList showsPrec1 showList1) 0 r . showString " }"
    where showList1 :: (Show1 g, Show a) => [g a] -> ShowS
          showList1 = liftShowList showsPrec showList

instance (Eq1 f, Eq t, Eq n) => Eq (CFG f t n) where
  CFG s1 r1 == CFG s2 r2 = s1 == s2 && liftEq (liftEq eq1) r1 r2

instance (Ord1 f, Ord t, Ord n) => Ord (CFG f t n) where
  compare (CFG s1 r1) (CFG s2 r2) = compare s1 s2 <> liftCompare (liftCompare compare1) r1 r2


data Symbol t n = T t | N n
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

symbol :: (t -> a) -> (n -> a) -> Symbol t n -> a
symbol f _ (T t) = f t
symbol _ g (N n) = g n

instance Bifunctor Symbol where
  bimap f g = symbol (T . f) (N . g)


data BiNF s = Z | U s | B (BiNF s) (BiNF s)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Show1 BiNF where
  liftShowsPrec sp _ = go
    where go d b = case b of
            Z     -> showString            "Z"
            U s   -> showsUnaryWith  sp    "U" d s
            B a b -> showsBinaryWith go go "B" d a b

instance Eq1 BiNF where
  liftEq eq = go
    where go Z         Z         = True
          go (U s1)    (U s2)    = eq s1 s2
          go (B a1 b1) (B a2 b2) = go a1 a2 && go b1 b2
          go _         _         = False

instance Ord1 BiNF where
  liftCompare compare = go
    where go Z         Z         = EQ
          go Z         _         = LT
          go (U s1)    (U s2)    = compare s1 s2
          go (U _)     _         = LT
          go (B a1 b1) (B a2 b2) = go a1 a2 <> go b1 b2
          go _         _         = GT
