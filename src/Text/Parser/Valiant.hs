module Text.Parser.Valiant where

data TwoNF t = U t | B (TwoNF t) (TwoNF t)

data Production t n = T t | N n
