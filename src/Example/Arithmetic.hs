module Example.Arithmetic where

import Control.Applicative
import Data.Recursive
import Text.Parser.Token

data Expr a
  = K a
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Exp (Expr a) (Expr a)
  | Abs (Expr a)
  | Sig (Expr a)
  deriving (Eq, Show)

instance Num a => Num (Expr a) where
  fromInteger = K . fromInteger
  (+) = Add
  (*) = Mul
  (-) = Sub
  abs = Abs
  signum = Sig


expr :: (Recursive m, TokenParsing m) => m (Expr Integer)
expr = mu (\ expr -> term expr `chainl1` (Add <$ symbol "+" <|> Sub <$ symbol "-"))

term :: (Recursive m, TokenParsing m) => m (Expr Integer) -> m (Expr Integer)
term expr = factor expr `chainl1` (Mul <$ symbol "*" <|> Div <$ symbol "/")

factor :: (Recursive m, TokenParsing m) => m (Expr Integer) -> m (Expr Integer)
factor expr = parens expr <|> K <$> integer
