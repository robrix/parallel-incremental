module Control.Higher.Comonad.Cofree where

data Cofree f a x = Cofree { extract :: a x, unwrap :: f (Cofree f a) x }
