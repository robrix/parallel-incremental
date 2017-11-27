{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
module Text.Parser.RecursiveDescent
( runGrammar
) where

import Data.Bifunctor
import Data.Grammar
import Data.Rec
import Data.List (intercalate)

type State t = [t]
type Error t = ([String], State t)

runGrammar :: Show t => (forall n . Rec (Grammar t) n a) -> State t -> Either String a
runGrammar grammar cs = first formatError $ do
  (a, cs) <- runK (iterRec algebra grammar) cs
  if null cs then
    Right a
  else
    Left  (["eof"], cs)
  where algebra :: (forall a . r a -> K t a) -> Grammar t r a -> K t a
        algebra go g = K $ \ cs -> case g of
          Err es -> Left (es, cs)
          Nul a -> Right (a, cs)
          Sat p | c:cs' <- cs, p c -> Right (c, cs')
                | otherwise        -> Left  ([], cs)
          Alt a b -> either (\ (e1, _) -> first (\ (e2, cs) -> (e1 ++ e2, cs)) (runK (go b) cs)) Right (runK (go a) cs)
          Seq f a b -> do
            (a', cs')  <- runK (go a) cs
            let fa = f a'
            (b', cs'') <- fa `seq` runK (go b) cs'
            let fab = fa b'
            fab `seq` Right (fab, cs'')
          Lab a s -> first (\ (_, cs) -> ([s], cs)) (runK (go a) cs)
          End | [] <- cs  -> Right ((), [])
              | otherwise -> Left  (["eof"], cs)

newtype K t a = K { runK :: State t -> Either (Error t) (a, State t) }

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
