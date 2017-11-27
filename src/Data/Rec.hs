{-# LANGUAGE GADTs #-}
module Data.Rec where

data Rec g n a where
  Var :: n a -> Rec g n a
  Rec :: (n a -> Rec g n a) -> Rec g n a
  In :: g (Rec g n) a -> Rec g n a
