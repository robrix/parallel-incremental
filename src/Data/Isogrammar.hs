{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
module Data.Isogrammar where

import Control.Applicative
import Control.Category
import Control.Monad
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

unit :: a <-> (a, ())
unit = Iso (\ a -> Just (a, ())) (Just . fst)

(***) :: (a <-> b) -> (c <-> d) -> ((a, c) <-> (b, d))
i *** j = Iso (\ (a, b) -> (,) <$> apply i a <*> apply j b) (\ (c, d) -> (,) <$> unapply i c <*> unapply j d)

driver :: (a -> Maybe a) -> a -> a
driver step state = case step state of
  Just state' -> driver step state'
  Nothing     -> state

iterate :: (a <-> a) -> (a <-> a)
iterate step = Iso f g
  where f = Just . driver (apply   step)
        g = Just . driver (unapply step)

step :: ((a, b) <-> a) -> ((a, [b]) <-> (a, [b]))
step i = (i *** id) . associate . (id *** inverse cons)

foldl :: ((a, b) <-> a) -> ((a, [b]) <-> a)
foldl i = inverse unit . (id *** inverse nil) . iterate (step i)


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


class Isofunctor f where
  (<#>) :: (a <-> b) -> f a -> f b
  infixr 4 <#>

  (<#) :: (a, b) -> f b -> f a
  (a, b) <# r = Iso (const (Just a)) (const (Just b)) <#> r

  infixr 4 <#

class Isofunctor f => Isoapplicative f where
  isopure :: a -> f a

  (<.>) :: f a -> f b -> f (a, b)
  infixr 4 <.>

  (<.) :: f a -> f () -> f a
  p <. q = inverse unit <#> p <.> q

  infixr 4 <.

  (.>) :: f () -> f a -> f a
  p .> q = inverse unit . commute <#> p <.> q

  infixr 4 .>

between :: Isoapplicative f => f () -> f () -> f a -> f a
between p q r = p .> r <. q


class Isoapplicative f => Isoalternative f where
  isoempty :: f a

  (<!>) :: f a -> f b -> f (These a b)
  infixr 3 <!>

  isomany :: f a -> f [a]

skipMany :: Isoalternative f => f a -> f ()
skipMany p = ((), []) <# isomany p

skipSome :: Isoalternative f => f () -> f ()
skipSome p = p .> skipMany p
