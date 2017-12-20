{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Higher.Function where

type a ~> b = forall x . a x -> b x

infixr 0 ~>
