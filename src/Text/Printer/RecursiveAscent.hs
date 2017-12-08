{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeOperators #-}
module Text.Printer.RecursiveAscent
( runCogrammar
) where

import Data.Higher.Function
import Data.Rec
import Data.Semigroup
import Data.These

data Cogrammar t r a
  = Err [String]
  | Nul a
  | Sat (a -> [t])
  | forall b c . Alt (a -> These c  b)  (r c) (r b)
  | forall b c . Seq (a ->      (c, b)) (r c) (r b)
  | Lab (r a) String
  | End a

runCogrammar :: Show t => (forall n . Rec n (Cogrammar t) a) -> a -> [[t]]
runCogrammar cogrammar a = runK (iterRec algebra cogrammar) a
  where algebra :: (r ~> K t) -> Cogrammar t r ~> K t
        algebra go g = K $ \ s -> case g of
          Err _ -> []
          Nul _ -> []
          Sat _ -> []
          Alt f a b -> mergeTheseWith (runK (go a)) (runK (go b)) (<>) (f s)
          Seq f a b -> let (s1, s2) = f s in (<>) <$> runK (go a) s1 <*> runK (go b) s2
          Lab a _ -> runK (go a) s
          End _ -> []

newtype K t a = K { runK :: a -> [[t]] }
