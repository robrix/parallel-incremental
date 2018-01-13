{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
module Data.Isogrammar where

import Control.Applicative
import Control.Category
import Data.Bifunctor
import Data.Higher.Function as H
import Data.Rec
import Data.Semigroup
import Data.These
import Prelude hiding ((.), id)

data Isogrammar t r a where
  Err :: [String] -> Isogrammar t r a
  Nul :: a -> Isogrammar t r a
  Sat :: (t <-> a) -> Isogrammar t r a
  Map :: (b <-> a) -> r b -> Isogrammar t r a
  Alt :: r a -> r b -> Isogrammar t r (These a  b)
  Seq :: r a -> r b -> Isogrammar t r (      a, b)
  Lab :: r a -> String -> Isogrammar t r a
  End :: Isogrammar t r ()


prettyPrint :: (forall n . Rec n (Isogrammar Char) a) -> a -> Maybe String
prettyPrint grammar a = toS <$> runK (iterRec algebra grammar) a
  where algebra :: (r ~> K) -> Isogrammar Char r ~> K
        algebra yield g = K $ \ a -> case g of
          Err _ -> Nothing
          Nul _ -> Just mempty
          Sat p -> char <$> unapply p a
          Map f b -> unapply f a >>= runK (yield b)
          Alt c b -> these (runK (yield c)) (runK (yield b)) (\ c' b' -> runK (yield c) c' <|> runK (yield b) b') a
          Seq c b -> uncurry (liftA2 (<>)) (bimap (runK (yield c)) (runK (yield b)) a)
          Lab r _ -> runK (yield r) a
          End -> Just mempty

newtype K a = K { runK :: a -> Maybe StringS }

newtype StringS = StringS { unStringS :: String -> String }

char :: Char -> StringS
char c = StringS (c:)

toS :: StringS -> String
toS = ($ "") . unStringS

instance Semigroup StringS where
  StringS a <> StringS b = StringS (a . b)

instance Monoid StringS where
  mempty = StringS id
  mappend = (<>)


data (a <-> b) = Iso { apply :: a -> Maybe b, unapply :: b -> Maybe a }

inverse :: (a <-> b) -> (b <-> a)
inverse (Iso f g) = Iso g f

associate :: ((a, (b, c)) <-> ((a, b), c))
associate = Iso f g
  where f (a, (b, c)) = Just ((a, b), c)
        g ((a, b), c) = Just (a, (b, c))

commute :: (a, b) <-> (b, a)
commute = Iso f f where f (a, b) = Just (b, a)


nil :: () <-> [a]
nil = Iso (const (Just [])) (\ l -> case l of
  [] -> Just ()
  _  -> Nothing)

cons :: (a, [a]) <-> [a]
cons = Iso (Just . uncurry (:)) (\ l -> case l of
  a:as -> Just (a, as)
  _    -> Nothing)


instance Category (<->) where
  id = Iso Just Just

  Iso f1 t1 . Iso f2 t2 = Iso (f1 <=< f2) (t2 <=< t1)

