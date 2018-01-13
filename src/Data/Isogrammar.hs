{-# LANGUAGE ExistentialQuantification, RankNTypes, TypeOperators #-}
module Data.Isogrammar where

import Control.Applicative
import Control.Category
import Data.Bifunctor
import Data.Higher.Function as H
import Data.Rec
import Data.Semigroup
import Data.These
import Prelude hiding ((.), id)

data Isogrammar t r a
  = Err [String]
  | Nul a
  | Sat (t <-> a)
  | forall c b . Alt (These  c  b  <-> a) (r c) (r b)
  | forall c b . Seq (      (c, b) <-> a) (r c) (r b)
  | Lab (r a) String
  | End a


prettyPrint :: (forall n . Rec n (Isogrammar Char) a) -> a -> Maybe String
prettyPrint grammar a = toS <$> runK (iterRec algebra grammar) a
  where algebra :: (r ~> K) -> Isogrammar Char r ~> K
        algebra yield g = K $ \ a -> case g of
          Err _ -> Nothing
          Nul _ -> Just mempty
          Sat p -> char <$> unapply p a
          Alt f c b -> unapply f a >>= these (runK (yield c)) (runK (yield b)) (\ c' b' -> runK (yield c) c' <|> runK (yield b) b')
          Seq f c b -> unapply f a >>= uncurry (<|>) . bimap (runK (yield c)) (runK (yield b))
          Lab r _ -> runK (yield r) a
          End _ -> Just mempty

newtype K a = K { runK :: a -> Maybe StringS }

newtype StringS = StringS { unStringS :: String -> String }

char :: Char -> StringS
char c = StringS (c:)

toS :: StringS -> String
toS = ($ "") . unStringS

instance Semigroup StringS where
  StringS a <> StringS b = StringS (b . a)

instance Monoid StringS where
  mempty = StringS id
  mappend = (<>)


data (a <-> b) = Iso { apply :: a -> Maybe b, unapply :: b -> Maybe a }

instance Category (<->) where
  id = Iso Just Just

  Iso f1 t1 . Iso f2 t2 = Iso (f1 <=< f2) (t2 <=< t1)

