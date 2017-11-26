module Text.Parser.RecursiveDescent where

import Data.List (intercalate)

type State s = [s]
type Error s = ([String], State s)

formatExpectation :: [String] -> String
formatExpectation [] = "eof"
formatExpectation [e1] = e1
formatExpectation [e1, e2] = e1 ++ " or " ++ e2
formatExpectation es = intercalate ", " (init es) ++ ", or " ++ last es
