{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeOperators #-}
module Text.Printer.RecursiveAscent
( runCogrammar
) where

import Data.Higher.Functor
import Data.Rec
import Data.These

data Cogrammar t r a
  = Err [String]
  | Nul a
  | Sat (a -> t)
  | forall b c . Alt (a -> These c  b)  (r c) (r b)
  | forall b c . Seq (a ->      (c, b)) (r c) (r b)
  | Lab (r a) String
  | End a

runCogrammar :: Show t => (forall n . Rec n (Cogrammar t) a) -> a -> [t]
runCogrammar cogrammar a = runK (iterRec algebra cogrammar) a
  where algebra :: (r ~> K t) -> Cogrammar t r ~> K t
        algebra go g = K $ \ s -> case g of
          Err _ -> []
          Nul _ -> []
          Sat _ -> []
          Alt _ _ _ -> []
          Seq _ _ _ -> []
          Lab a _ -> runK (go a) s
          End _ -> []

newtype K t a = K { runK :: a -> [t] }
