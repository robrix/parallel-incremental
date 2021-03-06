{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Text.Parser.Valiant where

import Data.Bifunctor
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Set as Set

data CFG f n t = CFG { start :: n, rules :: Map.Map n (Set.Set (f (Symbol n t))) }
  deriving (Foldable)

size :: (Foldable f, Num a) => CFG f t n -> a
size = getSum . foldMap (foldMap (fromIntegral . length)) . rules

nullableSymbols :: (Foldable f, Ord n) => CFG f n t -> Set.Set n
nullableSymbols = Map.foldMapWithKey (\ n f -> if any null f then Set.singleton n else Set.empty) . rules

lookup :: (Ord n, Ord (f (Symbol n t))) => n -> CFG f n t -> Set.Set (f (Symbol n t))
lookup sym = fromMaybe mempty . Map.lookup sym . rules


instance (Show1 f, Show t, Show n) => Show (CFG f n t) where
  showsPrec d (CFG s r) = showParen (d > 10) $ showString "CFG { start = " . showsPrec 0 s . showString ", rules" . liftShowsPrec (liftShowsPrec showsPrec1 showList1) (liftShowList showsPrec1 showList1) 0 r . showString " }"
    where showList1 :: (Show1 g, Show a) => [g a] -> ShowS
          showList1 = liftShowList showsPrec showList

instance (Eq1 f, Eq t, Eq n) => Eq (CFG f n t) where
  CFG s1 r1 == CFG s2 r2 = s1 == s2 && liftEq (liftEq eq1) r1 r2

instance (Ord1 f, Ord t, Ord n) => Ord (CFG f n t) where
  compare (CFG s1 r1) (CFG s2 r2) = compare s1 s2 <> liftCompare (liftCompare compare1) r1 r2


data Symbol n t = N n | T t
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

symbol :: (n -> a) -> (t -> a) -> Symbol n t -> a
symbol f _ (N n) = f n
symbol _ g (T t) = g t

instance Bifunctor Symbol where
  bimap f g = symbol (N . f) (T . g)


data BiNF s = Z | U s | B s s
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Show1 BiNF where
  liftShowsPrec sp _ d b = case b of
    Z     -> showString            "Z"
    U s   -> showsUnaryWith  sp    "U" d s
    B a b -> showsBinaryWith sp sp "B" d a b

instance Eq1 BiNF where
  liftEq eq a b = case (a, b) of
    (Z       , Z)       -> True
    (U s1    , U s2)    -> eq s1 s2
    (B a1 b1 , B a2 b2) -> eq a1 a2 && eq b1 b2
    _                   -> False

instance Ord1 BiNF where
  liftCompare compare a b = case (a, b) of
    (Z       , Z)       -> EQ
    (Z       , _)       -> LT
    (U s1    , U s2)    -> compare s1 s2
    (U _     , _)       -> LT
    (B a1 b1 , B a2 b2) -> compare a1 a2 <> compare b1 b2
    _                   -> GT
