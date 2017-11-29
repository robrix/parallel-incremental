{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor.Classes
( Show1(..)
, showsUnaryWith
, showsBinaryWith
, showsTernaryWith
) where

import Data.Functor.Classes (showsUnaryWith, showsBinaryWith)

class Show1 f where
  liftShowsPrec :: (forall x. Int -> a x -> ShowS) -> Int -> f a x -> ShowS

showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
showsTernaryWith sa sb sc con d a b c = showParen (d > 10) (showString con . showChar ' ' . sa 11 a . showChar ' ' . sb 11 b . showChar ' ' . sc 11 c)
