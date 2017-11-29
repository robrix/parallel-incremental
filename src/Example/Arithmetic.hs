module Example.Arithmetic where

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
