{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Text.Parser.RecursiveDescent
( runGrammar
) where

import qualified Control.Monad.State as M
import Data.Align
import Data.Bifunctor
import Data.Delta
import Data.Function (on)
import Data.Grammar
import Data.Higher.Functor as H
import Data.Rec
import Data.Result
import Data.These
import Data.List (intercalate)

data State t
  = State
    { stateOffset :: {-# UNPACK #-} !Delta
    , stateInput  ::                ![t]
    }

type Error t = ([String], State t)

runGrammar :: Show t => (forall n . Rec n (Grammar t) a) -> [t] -> Either [String] a
runGrammar grammar ts = result (Left . map formatError) Right $ do
  (a, s') <- M.runStateT (iterRec algebra grammar) (State mempty ts)
  if null (stateInput s') then
    Success a
  else
    Failure [(["eof"], s')]
  where algebra :: (r ~> M.StateT (State t) (Result (Error t))) -> Grammar t r ~> M.StateT (State t) (Result (Error t))
        algebra go g = case g of
          Err e -> do
            s <- M.get
            M.lift $ Failure [(e, s)]
          Nul a -> pure a
          Sat p -> do
            s <- M.get
            case stateInput s of
              c:cs' | Just a <- p c -> do
                M.put (s { stateInput = cs' })
                pure a
              _ -> M.lift $ Failure [([], s)]
          Alt f a b -> do
            s <- M.get
            (a, s') <- M.lift (alignWith (these (first (f . This)) (first (f . That)) (\ (a1, s1) (a2, s2) -> (f (These a1 a2), min s1 s2))) (M.runStateT (go a) s) (M.runStateT (go b) s))
            M.put s'
            pure a
          Seq f a b -> do
            a' <-          go a
            let fa = f a'
            b' <- fa `seq` go b
            let fab = fa b'
            pure $! fab
          Lab a l -> do
            s <- M.get
            (a, s') <- M.lift (first (\ (_, s) -> ([l], s)) (M.runStateT (go a) s))
            M.put s'
            pure a
          End a -> do
            s <- M.get
            case stateInput s of
              [] -> pure a
              _  -> M.lift $ Failure [(["eof"], s)]

formatError :: Show t => Error t -> String
formatError ([], (State _ [])) = "no rule to match at eof"
formatError ([], (State _ cs)) = "no rule to match at " ++ show cs
formatError (es, (State _ [])) = "expected " ++ formatExpectation es ++ " at eof"
formatError (es, (State _ cs)) = "expected " ++ formatExpectation es ++ " at " ++ show cs

formatExpectation :: [String] -> String
formatExpectation [] = "eof"
formatExpectation [e1] = e1
formatExpectation [e1, e2] = e1 ++ " or " ++ e2
formatExpectation es = intercalate ", " (init es) ++ ", or " ++ last es


instance Eq (State t) where
  (==) = (==) `on` stateOffset

instance Ord (State t) where
  compare = compare `on` stateOffset
