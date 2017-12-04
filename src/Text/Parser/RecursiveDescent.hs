{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
module Text.Parser.RecursiveDescent
( runGrammar
) where

import Data.Align
import Data.Bifunctor
import Data.Grammar
import Data.Ord (comparing)
import Data.Rec
import Data.Result
import Data.These
import Data.List (intercalate)

type State t = [t]
type Error t = ([String], State t)

runGrammar :: Show t => (forall n . Rec n (Grammar t) a) -> State t -> Either [String] a
runGrammar grammar cs = result (Left . map formatError) Right $ do
  (a, cs) <- runK (iterRec algebra grammar) cs
  if null cs then
    Success a
  else
    Failure [(["eof"], cs)]
  where algebra :: (forall a . r a -> K t a) -> Grammar t r a -> K t a
        algebra go g = K $ \ cs -> case g of
          Err es -> Failure [(es, cs)]
          Nul a -> Success (a, cs)
          Sat p | c:cs' <- cs, Just a <- p c -> Success (a, cs')
                | otherwise                  -> Failure [([], cs)]
          Alt f a b -> alignWith (mergeTheseWith (first (f . This)) (first (f . That)) (minBy (comparing length))) (runK (go a) cs) (runK (go b) cs)
          Seq f a b -> do
            (a', cs')  <- runK (go a) cs
            let fa = f a'
            (b', cs'') <- fa `seq` runK (go b) cs'
            let fab = fa b'
            fab `seq` Success (fab, cs'')
          Lab a s -> first (\ (_, cs) -> ([s], cs)) (runK (go a) cs)
          End a | [] <- cs  -> Success (a, [])
                | otherwise -> Failure [(["eof"], cs)]

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy c a b | GT <- c a b = b
            | otherwise   = a

newtype K t a = K { runK :: State t -> Result (Error t) (a, State t) }

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
