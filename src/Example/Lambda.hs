module Example.Lambda where

import Control.Applicative
import Text.Parser.Char
import Text.Parser.Token

data Lam = Abs String Lam | App Lam Lam | V String
  deriving (Eq, Show)

name :: TokenParsing m => m String
name = token ((:) <$> letter <*> many alphaNum)
