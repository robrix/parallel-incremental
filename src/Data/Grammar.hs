{-# LANGUAGE GADTs #-}
module Data.Grammar where

data Grammar s a where
  Nul :: a -> Grammar s a
  Lit :: s -> Grammar s s
  Alt :: Grammar s a -> Grammar s a -> Grammar s a
  Seq :: (a -> b -> c) -> Grammar s a -> Grammar s b -> Grammar s c
