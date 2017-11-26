{-# LANGUAGE GADTs, RankNTypes #-}
module Text.Parser.RecursiveDescent where

import Data.Bifunctor
import Data.Grammar
import Data.List (intercalate)
import Unsafe.Coerce (unsafeCoerce)

type State s = [s]
type Env s = [Binding s]
type Error s = ([String], State s)

runGrammar :: (Eq s, Show s) => (forall n . Grammar n s a) -> State s -> Either String a
runGrammar grammar cs = first formatError $ do
  (a, cs) <- go [] grammar cs
  if null cs then
    Right a
  else
    Left  (["eof"], cs)
  where go :: (Eq s, Show s) => Env s -> Grammar Name s a -> State s -> Either (Error s) (a, State s)
        go _ (Err es) cs = Left (es, cs)
        go _ (Nul a) cs = Right (a, cs)
        go _ (Sat p) cs
          | c:cs' <- cs, p c = Right (c, cs')
          | otherwise        = Left  ([], cs)
        go env (Alt a b) cs = either (\ (e1, _) -> first (\ (e2, cs) -> (e1 ++ e2, cs)) (go env b cs)) Right (go env a cs)
        go env (Seq f a b) cs = do
          (a', cs')  <- go env a cs
          let fa = f a'
          (b', cs'') <- fa `seq` go env b cs'
          let fab = fa b'
          fab `seq` Right (fab, cs'')
        go env (Lab a s) cs = first (\ (_, cs) -> ([s], cs)) (go env a cs)
        go _   End [] = Right ((), [])
        go _   End cs = Left  (["eof"], cs)
        go env (Var v) cs = (env ! v) cs
        go env (Rec r) cs = go (env ++ [Binding (go env (Rec r))]) (r (Name (length env))) cs

formatError :: Show s => Error s -> String
formatError ([], []) = "no rule to match at eof"
formatError ([], cs) = "no rule to match at " ++ show cs
formatError (es, []) = "expected " ++ formatExpectation es ++ " at eof"
formatError (es, cs) = "expected " ++ formatExpectation es ++ " at " ++ show cs

formatExpectation :: [String] -> String
formatExpectation [] = "eof"
formatExpectation [e1] = e1
formatExpectation [e1, e2] = e1 ++ " or " ++ e2
formatExpectation es = intercalate ", " (init es) ++ ", or " ++ last es

data Binding s where
  Binding :: (State s -> Either (Error s) (a, State s)) -> Binding s

newtype Name a = Name { getName :: Int }
  deriving (Eq, Show)

(!) :: Env s -> Name a -> State s -> Either (Error s) (a, State s)
(env ! Name n) s = go env n n s
  where go (Binding b : env) n n' s
          | n == 0    = unsafeCoerce (b s)
          | otherwise = go env (pred n) n' s
        go [] _ n' _ = error ("(!): " ++ show n' ++ " out of bounds")

infixl 0 !
