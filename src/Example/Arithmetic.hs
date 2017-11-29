module Example.Arithmetic where

import Control.Applicative
import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.Recursive
import Text.Parser.Token

data Expr
  = I Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  | Abs Expr
  | Sig Expr
  deriving (Eq, Show)

instance Num Expr where
  fromInteger = I
  (+) = Add
  (*) = Mul
  (-) = Sub
  abs = Abs
  signum = Sig


expr :: (Recursive m, TokenParsing m) => m Expr
expr = mu (\ expr -> term expr `chainl1` (Add <$ symbol "+" <|> Sub <$ symbol "-"))

term :: (Recursive m, TokenParsing m) => m Expr -> m Expr
term expr = factor expr `chainl1` (Mul <$ symbol "*" <|> Div <$ symbol "/")

factor :: (Recursive m, TokenParsing m) => m Expr -> m Expr
factor expr = app expr `chainl1` (Exp <$ symbol "^")

app :: (Recursive m, TokenParsing m) => m Expr -> m Expr
app expr = Abs <$ symbol "abs" <*> atom expr <|> atom expr

atom :: TokenParsing m => m Expr -> m Expr
atom expr = parens expr <|> I <$> integer


runExpr :: Expr -> Maybe Integer
runExpr (I a) = Just a
runExpr (Add a b) = (+) <$> runExpr a <*> runExpr b
runExpr (Mul a b) = (*) <$> runExpr a <*> runExpr b
runExpr (Sub a b) = (-) <$> runExpr a <*> runExpr b
runExpr (Div a b) = div <$> runExpr a <*> (runExpr b >>= nonZero)
runExpr (Exp a b) = do
  a' <- runExpr a
  b' <- runExpr b
  pure (foldl' (*) 1 (replicate (fromInteger b') a'))
runExpr (Abs a) = abs <$> runExpr a
runExpr (Sig a) = signum <$> runExpr a

nonZero :: Integer -> Maybe Integer
nonZero a = guard (a /= 0) *> pure a
