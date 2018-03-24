module Text.Parser.Valiant where

data TwoNF t = U t | B (TwoNF t) (TwoNF t)
