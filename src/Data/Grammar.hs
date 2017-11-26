{-# LANGUAGE GADTs #-}
module Data.Grammar where

data Grammar s a where
  Lit :: s -> Grammar s s
  Alt :: Grammar s a -> Grammar s a -> Grammar s a
  Seq :: Grammar s a -> Grammar s b -> Grammar s (a, b)
