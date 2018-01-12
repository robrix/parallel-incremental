{-# LANGUAGE ExistentialQuantification, TypeOperators #-}
module Data.Isogrammar where

import Control.Category
import Data.These
import Prelude hiding ((.), id)

data Isogrammar t r a
  = Err [String]
  | Nul a
  | Sat (t -> Maybe a)
  | forall c b . Alt (These c    b -> a) (r c) (r b)
  | forall c b . Seq (      c -> b -> a) (r c) (r b)
  | Lab (r a) String
  | End a

data (a <-> b) = Iso { from :: a -> b, to :: b -> a }

instance Category (<->) where
  id = Iso (\ x -> x) (\ x -> x)

  Iso f1 t1 . Iso f2 t2 = Iso (f1 . f2) (t2 . t1)
