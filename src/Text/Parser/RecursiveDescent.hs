module Text.Parser.RecursiveDescent where

type State s = [s]
type Error s = ([String], State s)
