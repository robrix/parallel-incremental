{-# LANGUAGE GADTs #-}
module Text.Parser.RecursiveDescent where

import Data.Functor.Const
import Data.List (intercalate)
import Unsafe.Coerce (unsafeCoerce)

type State s = [s]
type Error s = ([String], State s)

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

(!) :: [Binding s] -> Const Int a -> State s -> Either (Error s) (a, State s)
(env ! Const n) s = go env n n s
  where go (Binding b : env) n n' s
          | n == 0    = unsafeCoerce (b s)
          | otherwise = go env (pred n) n' s
        go [] _ n' _ = error ("(!): " ++ show n' ++ " out of bounds")

infixl 0 !
