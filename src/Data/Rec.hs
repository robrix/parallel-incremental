{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Data.Rec
( Rec(..)
, iterRec
) where

import Unsafe.Coerce

data Rec g n a where
  Var :: n a -> Rec g n a
  Rec :: (n a -> Rec g n a) -> Rec g n a
  In :: g (Rec g n) a -> Rec g n a


-- | Tear down a 'Rec' by iteration using an open-recursive algebra. Cycles are followed, unobservably.
iterRec :: forall a b g
        .  (  forall a r
           .  (forall a . r a -> b a)
           -> g r a
           -> b a
           )
        -> (forall n . Rec g n a)
        -> b a
iterRec algebra = go []
  where go :: Env b
           -> Rec g Name r
           -> b r
        go env (In g) = algebra (go env) g
        go env (Var v) = env ! v
        go env (Rec r) = let (name, env') = extend (go env (Rec r)) env in go env' (r name)


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
