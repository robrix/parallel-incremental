{-# LANGUAGE FlexibleInstances, GADTs, RankNTypes, TypeOperators #-}
module Data.Isogrammar where

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.DList
import Data.Foldable (toList)
import Data.Higher.Function as H
import Data.List (intercalate)
import Data.Rec
import Data.Result
import Data.Semigroup
import Prelude hiding ((.), foldl, id, iterate)

data Isogrammar t r a where
  Err :: [String] -> Isogrammar t r a
  Nul :: a -> Isogrammar t r a
  Sat :: (t <-> a) -> Isogrammar t r a
  Map :: (b <-> a) -> r b -> Isogrammar t r a
  Alt :: r a -> r a -> Isogrammar t r a
  Seq :: r a -> r b -> Isogrammar t r (a, b)
  Lab :: r a -> String -> Isogrammar t r a
  End :: Isogrammar t r ()


prettyPrint :: (forall n . Rec n (Isogrammar t) a) -> a -> Maybe [t]
prettyPrint grammar a = toList <$> runK (iterRec algebra grammar) a
  where algebra :: (r ~> K t) -> Isogrammar t r ~> K t
        algebra yield g = K $ \ a -> case g of
          Err _ -> Nothing
          Nul _ -> Just mempty
          Sat p -> pure <$> unapply p a
          Map f b -> unapply f a >>= runK (yield b)
          Alt c b -> runK (yield c) a <|> runK (yield b) a
          Seq c b -> uncurry (liftA2 (<>)) (bimap (runK (yield c)) (runK (yield b)) a)
          Lab r _ -> runK (yield r) a
          End -> Just mempty

newtype K t a = K { runK :: a -> Maybe (DList t) }


data (a <-> b) = Iso { apply :: a -> Maybe b, unapply :: b -> Maybe a }

inverse :: (a <-> b) -> (b <-> a)
inverse (Iso f g) = Iso g f

associate :: ((a, (b, c)) <-> ((a, b), c))
associate = Iso f g
  where f (a, (b, c)) = pure ((a, b), c)
        g ((a, b), c) = pure (a, (b, c))

commute :: (a, b) <-> (b, a)
commute = Iso f f where f (a, b) = pure (b, a)

unit :: a <-> (a, ())
unit = Iso (\ a -> pure (a, ())) (pure . fst)

(***) :: (a <-> b) -> (c <-> d) -> ((a, c) <-> (b, d))
i *** j = Iso (\ (a, b) -> (,) <$> apply i a <*> apply j b) (\ (c, d) -> (,) <$> unapply i c <*> unapply j d)

driver :: (a -> Maybe a) -> a -> a
driver step state = case step state of
  Just state' -> driver step state'
  Nothing     -> state

iterate :: (a <-> a) -> (a <-> a)
iterate step = Iso f g
  where f = pure . driver (apply   step)
        g = pure . driver (unapply step)

step :: ((a, b) <-> a) -> ((a, [b]) <-> (a, [b]))
step i = (i *** id) . associate . (id *** inverse cons)

foldl :: ((a, b) <-> a) -> ((a, [b]) <-> a)
foldl i = inverse unit . (id *** inverse nil) . iterate (step i)


satisfy :: (Char -> Bool) -> Rec n (Isogrammar Char) Char
satisfy p = In (Sat (Iso (\ c -> guard (p c) *> pure c) pure))

(<?>) :: Rec n (Isogrammar Char) a -> String -> Rec n (Isogrammar Char) a
g <?> s = In (Lab g s)

infixl 0 <?>


alphaNum :: Rec n (Isogrammar Char) Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"

letter :: Rec n (Isogrammar Char) Char
letter = satisfy isAlpha <?> "letter"


token :: Rec n (Isogrammar Char) a -> Rec n (Isogrammar Char) a
token p = p <. skipMany (satisfy isSpace)

char :: Char -> Rec n (Isogrammar Char) ()
char c = In (Sat (Iso (\ c' -> guard (c == c') *> pure ()) (const (pure c))))

symbolic :: Char -> Rec n (Isogrammar Char) ()
symbolic = token . char


dot :: Rec n (Isogrammar Char) ()
dot = symbolic '.'

parens :: Rec n (Isogrammar Char) a -> Rec n (Isogrammar Char) a
parens g = between (symbolic '(') (symbolic ')') g


nil :: () <-> [a]
nil = Iso (const (pure [])) (\ l -> case l of
  [] -> pure ()
  _  -> empty)

cons :: (a, [a]) <-> [a]
cons = Iso (pure . uncurry (:)) (\ l -> case l of
  a:as -> pure (a, as)
  _    -> empty)


instance Category (<->) where
  id = Iso pure pure

  Iso f1 t1 . Iso f2 t2 = Iso (f1 <=< f2) (t2 <=< t1)


data Lam = Abs String Lam | App Lam Lam | V String
  deriving (Eq, Show)

mkAbs :: (String, Lam) <-> Lam
mkAbs = Iso (pure . uncurry Abs) (\ a -> case a of
  Abs n b -> pure (n, b)
  _       -> empty)

mkApp :: (Lam, Lam) <-> Lam
mkApp = Iso (pure . uncurry App) (\ a -> case a of
  App a b -> pure (a, b)
  _       -> empty)

mkVar :: String <-> Lam
mkVar = Iso (pure . V) (\ a -> case a of
  V s -> pure s
  _   -> empty)


lam :: Rec n (Isogrammar Char) Lam
lam = mu1 (\ lam -> abs' lam <!> app lam)

var :: Rec n (Isogrammar Char) Lam
var = mkVar <#> name <?> "variable"

name :: Rec n (Isogrammar Char) String
name = token (cons <#> letter <.> isomany alphaNum)

abs' :: Rec n (Isogrammar Char) Lam -> Rec n (Isogrammar Char) Lam
abs' lam = symbolic '\\' .> mkAbs <#> name <.> dot .> lam <?> "abstraction"

app :: Rec n (Isogrammar Char) Lam -> Rec n (Isogrammar Char) Lam
app lam = chainl1 (var <!> parens lam) (isopure ()) (mkApp . (id *** (inverse unit . commute))) <?> "application"

class Isofunctor f where
  (<#>) :: (a <-> b) -> f a -> f b
  infixr 4 <#>

  (<#) :: (a, b) -> f b -> f a
  (a, b) <# r = Iso (const (pure a)) (const (pure b)) <#> r

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

  (<!>) :: f a -> f a -> f a
  infixr 3 <!>

  isomany :: f a -> f [a]

skipMany :: Isoalternative f => f a -> f ()
skipMany p = ((), []) <# isomany p

chainl1 :: Isoalternative f => f a -> f b -> ((a,(b,a)) <-> a) -> f a
chainl1 arg op f = foldl f <#> arg <.> isomany (op <.> arg)


instance Isofunctor (Rec n (Isogrammar Char)) where
  f <#> a = In (Map f a)

instance Isoapplicative (Rec n (Isogrammar Char)) where
  isopure = In . Nul

  a <.> b = In (Seq a b)

instance Isoalternative (Rec n (Isogrammar Char)) where
  isoempty = In (Err [])

  a <!> b = In (Alt a b)

  isomany a = mu1 (\ more -> cons <#> a <.> more <!> isopure [])


type Error t = ([String], [t])

parse :: Show t => (forall n . Rec n (Isogrammar t) a) -> [t] -> Either [String] a
parse grammar ts = result (Left . map formatError) Right $ do
  (a, s') <- runParser (iterRec algebra grammar) ts
  if null s' then
    Success a
  else
    Failure [(["eof"], s')]
  where algebra :: (r ~> Parser t) -> Isogrammar t r ~> Parser t
        algebra yield g = Parser $ \ s -> case g of
          Err e -> Failure [(e, s)]
          Nul a -> pure (a, s)
          Sat p | c:cs <- s, Just a <- apply p c -> pure (a, cs)
                | otherwise                      -> Failure [([], s)]
          Map f a -> do
            (a', s') <- runParser (yield a) s
            maybe empty (pure . flip (,) s') (apply f a')
          Alt a b -> runParser (yield a) s <|> runParser (yield b) s
          Seq a b -> do
            (a', s')  <- runParser (yield a) s
            (b', s'') <- runParser (yield b) s'
            pure ((a', b'), s'')
          Lab a l -> first (\ (_, s) -> ([l], s)) (runParser (yield a) s)
          End | null s    -> pure ((), s)
              | otherwise -> Failure [(["eof"], s)]

newtype Parser t a = Parser { runParser :: [t] -> Result (Error t) (a, [t]) }

instance Functor (Parser t) where
  fmap f (Parser run) = Parser (\ ts -> first f <$> run ts)

formatError :: Show t => Error t -> String
formatError ([], []) = "no rule to match at eof"
formatError ([], cs) = "no rule to match at " ++ show cs
formatError (es, []) = "expected " ++ formatExpectation es ++ " at eof"
formatError (es, cs) = "expected " ++ formatExpectation es ++ " at " ++ show cs

formatExpectation :: [String] -> String
formatExpectation [] = "eof"
formatExpectation [e1] = e1
formatExpectation [e1, e2] = e1 ++ " or " ++ e2
formatExpectation es = intercalate ", " (init es) ++ ", or " ++ last es
