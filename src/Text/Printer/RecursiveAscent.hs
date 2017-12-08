{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Text.Printer.RecursiveAscent
( runGrammar
) where

import Data.Rec
import Data.These

data Cogrammar t r a
  = Err [String]
  | Nul a
  | Sat (t -> Maybe a)
  | forall c b . Alt (a -> These c  b)  (r c) (r b)
  | forall c b . Seq (a ->      (c, b)) (r c) (r b)
  | Lab (r a) String
  | End a

runGrammar :: Show t => (forall n . Rec n (Cogrammar t) a) -> a -> [t]
runGrammar _ _ = []
