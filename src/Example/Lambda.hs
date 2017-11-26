module Example.Lambda where

import Control.Applicative
import Data.Grammar hiding (Var)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Recursive
import Text.Parser.Token

data Lam = Abs String Lam | App Lam Lam | Var String
  deriving (Eq, Show)

lam :: Grammar n Char Lam
lam = mu (\ lam -> var <|> abs' lam <|> app lam)

var :: TokenParsing m => m Lam
var = Var <$> name <?> "variable"

name :: TokenParsing m => m String
name = token ((:) <$> letter <*> many alphaNum)

abs' :: TokenParsing m => m Lam -> m Lam
abs' lam = parens (Abs <$ symbolic '\\' <*> name <* dot <*> lam) <?> "abstraction"

app :: TokenParsing m => m Lam -> m Lam
app lam = parens (App <$> lam <*> lam) <?> "application"
