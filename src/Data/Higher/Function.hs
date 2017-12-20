{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Function where

type a ~> b = forall x . a x -> b x

infixr 0 ~>


newtype (a :-> b) x = A { unA :: a x -> b x }

infixr 0 :->
