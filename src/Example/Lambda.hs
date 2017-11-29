module Example.Lambda where

import Control.Applicative
import Data.Recursive
import Text.Parser.Char
import Text.Parser.Combinators hiding (chainl1)
import Text.Parser.Token

data Lam = Abs String Lam | App Lam Lam | Var String
  deriving (Eq, Show)

lam :: (Recursive m, TokenParsing m) => m Lam
lam = mu (\ lam -> abs' lam <|> app lam)

var :: TokenParsing m => m Lam
var = Var <$> name <?> "variable"

name :: TokenParsing m => m String
name = token ((:) <$> letter <*> many alphaNum)

abs' :: TokenParsing m => m Lam -> m Lam
abs' lam = Abs <$ symbolic '\\' <*> name <* dot <*> lam <?> "abstraction"

app :: (Recursive m, TokenParsing m) => m Lam -> m Lam
app lam = (var <|> parens lam) `chainl1` pure App <?> "application"
