module Example.Lambda where

data Lam = Abs String Lam | App Lam Lam | V String
  deriving (Eq, Show)
