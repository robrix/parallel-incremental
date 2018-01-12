{-# LANGUAGE ExistentialQuantification, TypeOperators #-}
module Data.Isogrammar where

import Data.These

data Isogrammar t r a
  = Err [String]
  | Nul a
  | Sat (t -> Maybe a)
  | forall c b . Alt (These c    b -> a) (r c) (r b)
  | forall c b . Seq (      c -> b -> a) (r c) (r b)
  | Lab (r a) String
  | End a

data (a <-> b) = Iso { from :: a -> b, to :: b -> a }
