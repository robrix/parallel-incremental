module Data.Result where

data Result e a
  = Failure [e]
  | Success a
