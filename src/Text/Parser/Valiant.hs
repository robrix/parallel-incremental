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
  showsPrec d (CFG s r) = showParen (d > 0) $ showString "CFG { start = " . showsPrec 0 s . showString ", rules" . liftShowsPrec (liftShowsPrec showsPrec1 showList1) (liftShowList showsPrec1 showList1) 0 r . showString " }"
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


data BiNF s = U s | B (BiNF s) (BiNF s)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
