{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, TypeOperators #-}
module Text.Parser.RecursiveDescent
( runGrammar
) where

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
  (a, s') <- runParser (iterRec algebra grammar) (State mempty ts)
  if null (stateInput s') then
    Success a
  else
    Failure [(["eof"], s')]
  where algebra :: (r ~> Parser t) -> Grammar t r ~> Parser t
        algebra go g = Parser $ \ s -> case g of
          Err e -> Failure [(e, s)]
          Nul a -> pure (a, s)
          Sat p -> case stateInput s of
            c:cs' | Just a <- p c -> pure (a, s { stateInput = cs' })
            _                     -> Failure [([], s)]
          Alt f a b -> alignWith (these (first (f . This)) (first (f . That)) (\ (a1, s1) (a2, s2) -> (f (These a1 a2), min s1 s2))) (runParser (go a) s) (runParser (go b) s)
          Seq f a b -> do
            (a', s')  <-          runParser (go a) s
            let fa = f a'
            (b', s'') <- fa `seq` runParser (go b) s'
            let fab = fa b'
            fab             `seq` pure (fab, s'')
          Lab a l -> first (\ (_, s) -> ([l], s)) (runParser (go a) s)
          End a -> case stateInput s of
            [] -> pure (a, s)
            _  -> Failure [(["eof"], s)]

newtype Parser t a = Parser { runParser :: State t -> Result (Error t) (a, State t) }

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
