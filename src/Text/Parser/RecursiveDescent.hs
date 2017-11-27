{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
module Text.Parser.RecursiveDescent where

import Data.Bifunctor
import Data.Grammar
import Data.List (intercalate)
import Unsafe.Coerce (unsafeCoerce)

type State t = [t]
type Error t = ([String], State t)

runGrammar :: (Eq t, Show t) => (forall n . Rec (Grammar t) n a) -> State t -> Either String a
runGrammar grammar cs = first formatError $ do
  (a, cs) <- iterRec algebra grammar cs
  if null cs then
    Right a
  else
    Left  (["eof"], cs)
  where algebra :: (Eq t, Show t) => (forall a . r a -> State t -> Either (Error t) (a, State t)) -> Grammar t r a -> State t -> Either (Error t) (a, State t)
        algebra go g cs = case g of
          Err es -> Left (es, cs)
          Nul a -> Right (a, cs)
          Sat p | c:cs' <- cs, p c -> Right (c, cs')
                | otherwise        -> Left  ([], cs)
          Alt a b -> either (\ (e1, _) -> first (\ (e2, cs) -> (e1 ++ e2, cs)) (go b cs)) Right (go a cs)
          Seq f a b -> do
            (a', cs')  <- go a cs
            let fa = f a'
            (b', cs'') <- fa `seq` go b cs'
            let fab = fa b'
            fab `seq` Right (fab, cs'')
          Lab a s -> first (\ (_, cs) -> ([s], cs)) (go a cs)
          End | [] <- cs  -> Right ((), [])
              | otherwise -> Left  (["eof"], cs)

iterRec :: forall a t
        .  (  forall a r
           .  (forall a . r a -> State t -> Either (Error t) (a, State t))
           -> Grammar t r a
           -> State t
           -> Either (Error t) (a, State t)
           )
        -> (forall n . Rec (Grammar t) n a)
        -> State t
        -> Either (Error t) (a, State t)
iterRec algebra = go mempty
  where go :: Env t
           -> Rec (Grammar t) Name r
           -> State t
           -> Either (Error t) (r, State t)
        go env (In g) cs = algebra (go env) g cs
        go env (Var v) cs = (env ! v) cs
        go env (Rec r) cs = let (name, env') = extend (go env (Rec r)) env in go env' (r name) cs

formatError :: Show t => Error t -> String
formatError ([], []) = "no rule to match at eof"
formatError ([], cs) = "no rule to match at " ++ show cs
formatError (es, []) = "expected " ++ formatExpectation es ++ " at eof"
formatError (es, cs) = "expected " ++ formatExpectation es ++ " at " ++ show cs

formatExpectation :: [String] -> String
formatExpectation [] = "eof"
formatExpectation [e1] = e1
formatExpectation [e1, e2] = e1 ++ " or " ++ e2
formatExpectation es = intercalate ", " (init es) ++ ", or " ++ last es

newtype Env t = Env { getEnv :: [Binding t] }
  deriving (Monoid)

data Binding t where
  Binding :: (State t -> Either (Error t) (a, State t)) -> Binding t

newtype Name a = Name { getName :: Int }
  deriving (Eq, Show)

(!) :: Env t -> Name a -> State t -> Either (Error t) (a, State t)
(Env env ! Name n) s = go env (pred (length env)) n s
  where go (Binding b : env) n n' s
          | n == n'   = unsafeCoerce (b s)
          | otherwise = go env (pred n) n' s
        go [] _ n' _ = error ("(!): " ++ show n' ++ " out of bounds")

infixl 0 !

extend :: (State t -> Either (Error t) (a, State t)) -> Env t -> (Name a, Env t)
extend cont (Env bs) = (Name (length bs), Env (Binding cont : bs))
