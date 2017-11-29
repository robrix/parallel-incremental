{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Data.Rec
( Rec(..)
, iterRec
, foldRec
, cata
) where

import Data.Higher.Foldable as H
import Data.Higher.Functor as H
import Data.Higher.Functor.Classes
import Data.Functor.Const
import Data.Recursive
import Unsafe.Coerce

-- | Lift a grammar @g@ indexed by types @a@ into a recursive grammar with nonterminals @n@.
data Rec n g a where
  Var :: n a -> Rec n g a
  Mu :: (n a -> Rec n g a) -> Rec n g a
  In :: g (Rec n g) a -> Rec n g a


-- | Tear down a 'Rec' by iteration using an open-recursive algebra. Cycles are followed, unobservably.
iterRec :: forall a b g
        .  (  forall r
           .  (r ~> b)
           -> g r
           ~> b
           )
        -> (forall n . Rec n g a)
        -> b a
iterRec algebra = go []
  where go :: Env b
           -> Rec Name g r
           -> b r
        go env (In g) = algebra (go env) g
        go env (Var v) = env ! v
        go env (Mu r) = let (name, env') = extend (go env (Mu r)) env in go env' (r name)

-- | Fold a 'Rec' by iteration using an open-recursive algebra. Cycles are indicated by the presence of a supplied seed value.
foldRec :: forall a b g
        .  (  forall r
           .  (r ~> b)
           -> g r
           ~> b
           )
        -> (forall a . b a)
        -> (forall n . Rec n g a)
        -> b a
foldRec alg seed = go
  where go :: Rec b g x -> b x
        go (In g) = alg go g
        go (Var v) = v
        go (Mu r) = go (r seed)


-- | Fold a 'Rec' using a higher-order F-algebra. Cycles are indicated by the presence of a supplied seed value.
cata :: forall f a . H.Functor f => (f a ~> a) -> (forall x . a x) -> Rec a f ~> a
cata alg seed = go
  where go :: Rec a f ~> a
        go (Var v) = v
        go (Mu r) = go (r seed)
        go (In g) = alg (H.fmap go g)


type Env b = [Binding b]

data Binding b where
  Binding :: b a -> Binding b

newtype Name a = Name Int
  deriving (Eq, Show)

(!) :: Env b -> Name a -> b a
env ! Name n = go env (pred (length env)) n
  where go (Binding b : env) n n'
          | n == n'   = unsafeCoerce b
          | otherwise = go env (pred n) n'
        go [] _ n' = error ("(!): " ++ show n' ++ " out of bounds")

infixl 0 !

extend :: b a -> Env b -> (Name a, Env b)
extend cont bs = (Name (length bs), Binding cont : bs)


instance Recursive (Rec n g) where
  mu f = Mu (f . Var)

instance Embed (Rec n) where
  embed = In

instance HShow1 f => Show (Rec (Const Char) f a)
  where showsPrec = showsRec 0 (iterate succ 'a')

showsRec :: HShow1 f => Int -> String -> Int -> Rec (Const Char) f a -> ShowS
showsRec indent s n r = case r of
  Var c -> showChar (getConst c)
  Mu g  -> showString "Mu (\\ " . showChar (head s) . showString " ->\n"
    . showString (replicate (2 * (succ indent)) ' ') . showsRec (succ indent) (tail s) n (g (Const (head s))) . showString ")"
  In fa -> hliftShowsPrec (showsRec indent s) n fa
