{-# LANGUAGE RankNTypes #-}
module Text.Printer.RecursiveAscent
( runGrammar
) where

import Data.Grammar
import Data.Rec

runGrammar :: Show t => (forall n . Rec n (Grammar t) a) -> a -> [t]
runGrammar _ _ = []
