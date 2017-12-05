{-# LANGUAGE DeriveFunctor, GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
module Text.Parser.RecursiveDescent
( runGrammar
) where

import Data.Align
import Data.Bifunctor
import Data.Delta
import Data.Function (on)
import Data.Grammar
import Data.Ord (comparing)
import Data.Rec
import Data.Result
import Data.These
import Data.List (intercalate)

data State t
  = State
    { stateOffset :: {-# UNPACK #-} !Delta
    , stateInput  ::                ![t]
    }
  deriving (Functor)

type Error t = ([String], State t)

runGrammar :: Show t => (forall n . Rec n (Grammar t) a) -> [t] -> Either [String] a
runGrammar grammar ts = result (Left . map formatError) Right $ do
  (a, s') <- runK (iterRec algebra grammar) (State mempty ts)
  if null (stateInput s') then
    Success a
  else
    Failure [(["eof"], s')]
  where algebra :: (forall a . r a -> K t a) -> Grammar t r a -> K t a
        algebra go g = K $ \ s -> case g of
          Err e -> Failure [(e, s)]
          Nul a -> Success (a, s)
          Sat p | c:cs' <- stateInput s, Just a <- p c -> Success (a, s { stateInput = cs' })
                | otherwise                            -> Failure [([], s)]
          Alt f a b -> alignWith (these (first (f . This)) (first (f . That)) (\ (a1, s1) (a2, s2) -> (f (These a1 a2), maxBy (comparing stateOffset) s1 s2))) (runK (go a) s) (runK (go b) s)
          Seq f a b -> do
            (a', s')  <- runK (go a) s
            let fa = f a'
            (b', s'') <- fa `seq` runK (go b) s'
            let fab = fa b'
            fab `seq` Success (fab, s'')
          Lab a l -> first (\ (_, s) -> ([l], s)) (runK (go a) s)
          End a | [] <- stateInput s -> Success (a, s)
                | otherwise          -> Failure [(["eof"], s)]

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy c a b | LT <- c a b = b
            | otherwise   = a

newtype K t a = K { runK :: State t -> Result (Error t) (a, State t) }

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

instance Align State where
  nil = State mempty []

  alignWith f (State o1 i1) (State o2 i2) = State (max o1 o2) (alignWith f i1 i2)
