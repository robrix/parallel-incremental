module Example.Lambda where

import Control.Applicative
import Data.Recursive
import Text.Parser.Char
import Text.Parser.Combinators hiding (chainl1)
import Text.Parser.Token

data Lam = Abs String Lam | App Lam Lam | Var String
  deriving (Eq, Show)

lam :: (Mu1 m, TokenParsing m) => m Lam
lam = mu1 (\ lam -> abs' lam <|> app lam)

var :: TokenParsing m => m Lam
var = Var <$> name <?> "variable"

name :: TokenParsing m => m String
name = token ((:) <$> letter <*> many alphaNum)

abs' :: TokenParsing m => m Lam -> m Lam
abs' lam = Abs <$ symbolic '\\' <*> name <* dot <*> lam <?> "abstraction"

app :: (Mu1 m, TokenParsing m) => m Lam -> m Lam
app lam = (var <|> parens lam) `chainl1` pure App <?> "application"
