module Example.Lambda where

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

data Lam = Abs String Lam | App Lam Lam | Var String
  deriving (Eq, Show)

var :: TokenParsing m => m Lam
var = Var <$> name <?> "variable"

name :: TokenParsing m => m String
name = token ((:) <$> letter <*> many alphaNum)

abs' :: TokenParsing m => m Lam -> m Lam
abs' lam = parens (Abs <$ symbolic '\\' <*> name <* dot <*> lam) <?> "abstraction"
