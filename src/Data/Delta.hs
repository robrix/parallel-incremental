module Data.Delta where

data Delta
  = Delta
    { deltaLines   :: {-# UNPACK #-} !Int
    , deltaColumns :: {-# UNPACK #-} !Int
    , deltaBytes   :: {-# UNPACK #-} !Int
    }
