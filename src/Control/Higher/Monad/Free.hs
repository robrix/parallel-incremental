module Control.Higher.Monad.Free where

data Free f a x = Pure (a x) | Free (f (Free f a) x)
