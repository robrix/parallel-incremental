{-# LANGUAGE RankNTypes #-}
module Data.Higher.Functor.Classes where

class HShow1 f where
  hliftShowsPrec :: (forall x. Int -> a x -> ShowS) -> Int -> f a x -> ShowS
