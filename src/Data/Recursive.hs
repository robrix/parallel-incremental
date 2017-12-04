module Data.Recursive
( Recursive(..)
, Embed(..)
, chainl1
) where

import Control.Applicative

class Recursive m where
  mu :: (m a -> m a) -> m a

class Embed t where
  embed1 :: f (t f) a -> t f a


chainl1 :: (Alternative m, Recursive m) => m a -> m (a -> a -> a) -> m a
chainl1 expr op = scan
  where scan = expr <**> rst
        rst = mu (\ more -> (\f y g x -> g (f x y)) <$> op <*> expr <*> more <|> pure id)
