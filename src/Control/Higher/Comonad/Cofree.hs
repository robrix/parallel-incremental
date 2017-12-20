{-# LANGUAGE PolyKinds #-}
module Control.Higher.Comonad.Cofree where

data Cofree f a x = Cofree { extract :: a x, unwrap :: f (Cofree f a) x }

data CofreeF f a b x = CofreeF { headF :: a x, tailF :: f b x }
