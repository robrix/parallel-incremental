{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Grammar
( Grammar(..)
, toGraph
) where

import qualified Algebra.Graph.Class as G
import Control.Applicative
import qualified Control.Higher.Applicative as H
import Control.Monad (guard)
import Control.Monad.State
import Data.Graph
import qualified Data.Higher.Foldable as H
import qualified Data.Higher.Functor as H
import qualified Data.Higher.Monoid as H
import Data.Higher.Functor.Classes as H
import Data.Higher.Functor.Foldable
import qualified Data.Higher.Traversable as H
import Data.Rec
import Data.These
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

data Grammar t r a
  = Err [String]
  | Nul a
  | Sat (t -> Maybe a)
  | forall c b . Alt (These c    b -> a) (r c) (r b)
  | forall c b . Seq (      c -> b -> a) (r c) (r b)
  | Lab (r a) String
  | End a

v :: a -> State Int (Graph a)
v a = StateT (\ i -> pure (G.vertex (Vertex i (Just a)), succ i))

toGraph :: (forall n . Rec n (Grammar t) a) -> Graph String
toGraph g = evalState (go G.empty g) 0
  where go :: Graph String -> Rec (Const (Graph String)) (Grammar t) x -> State Int (Graph String)
        go parent (Var v) = pure (parent >< (getConst v))
        go parent (Mu g) = do
          i <- get
          go parent (g (Const (Graph [Vertex i Nothing] [])))
        go parent (In r) = case r of
          Err _     -> (parent ><) <$> v "Err"
          Nul _     -> (parent ><) <$> v "Nul"
          Sat _     -> (parent ><) <$> v "Sat"
          Alt _ a b -> do
            alt <- v "Alt"
            a' <- go alt a
            b' <- go alt b
            pure (parent >< alt <> a' <> b')
          Seq _ a b -> do
            seq <- v "Seq"
            a' <- go seq a
            b' <- go seq b
            pure (parent >< seq <> a' <> b')
          Lab a s   -> do
            lab <- v ("Lab " ++ show s)
            a' <- go lab a
            pure (parent >< lab <> a')
          End _     -> (parent ><) <$> v "End"


instance (Bounded t, Enum t, Show t) => H.Show1 (Grammar t) where
  liftShowsPrec sp d g = case g of
    Err es    -> showsUnaryWith   showsPrec              "Err" d es
    Nul a     -> showsUnaryWith   hide                   "Nul" d a
    Sat p     -> showsUnaryWith   hide                   "Sat" d p
    Alt f a b -> showsTernaryWith hide      sp        sp "Alt" d f a b
    Seq f a b -> showsTernaryWith hide      sp        sp "Seq" d f a b
    Lab r s   -> showsBinaryWith  sp        showsPrec    "Lab" d r s
    End a     -> showsUnaryWith   hide                   "End" d a
    where hide _ _ = showChar '_'

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t) => Functor (r (Grammar t)) where
  fmap = liftA

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t) => Applicative (r (Grammar t)) where
  pure = embed1 . Nul
  liftA2 f a b = embed1 (Seq f a b)

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t, Mu1 (r (Grammar t))) => Alternative (r (Grammar t)) where
  empty = embed1 (Err [])
  a <|> b = embed1 (Alt (mergeThese const) a b)
  many a = mu1 (\ more -> (:) <$> a <*> more <|> pure [])
  some a = (:) <$> a <*> many a

instance (Corecursive1 (r (Grammar t)), Cobase1 (r (Grammar t)) ~ Grammar t, Mu1 (r (Grammar t))) => Parsing (r (Grammar t)) where
  try = id
  a <?> s = embed1 (Lab a s)
  eof = embed1 (End ())
  unexpected s = embed1 (Err [s])
  notFollowedBy a = a *> embed1 (Err ["unexpected"]) <|> pure ()

instance (Corecursive1 (r (Grammar Char)), Cobase1 (r (Grammar Char)) ~ Grammar Char, Mu1 (r (Grammar Char))) => CharParsing (r (Grammar Char)) where
  satisfy p = embed1 (Sat (\ c -> guard (p c) *> Just c))

instance (Corecursive1 (r (Grammar Char)), Cobase1 (r (Grammar Char)) ~ Grammar Char, Mu1 (r (Grammar Char))) => TokenParsing (r (Grammar Char))

instance H.Foldable (Grammar t) where
  foldMap f g = case g of
    Err _     -> H.mempty
    Nul _     -> H.mempty
    Sat _     -> H.mempty
    Alt _ a b -> f a H.<> f b
    Seq _ a b -> f a H.<> f b
    Lab r _   -> f r
    End _     -> H.mempty

instance H.Functor (Grammar t) where
  fmap f g = case g of
    Err es    -> Err es
    Nul a     -> Nul a
    Sat p     -> Sat p
    Alt g a b -> Alt g (f a) (f b)
    Seq g a b -> Seq g (f a) (f b)
    Lab r s   -> Lab (f r) s
    End a     -> End a

instance H.Traversable (Grammar t) where
  traverse f g = case g of
    Err es    -> H.pure (Err es)
    Nul a     -> H.pure (Nul a)
    Sat p     -> H.pure (Sat p)
    Alt g a b -> H.liftA2 (Alt g) (f a) (f b)
    Seq g a b -> H.liftA2 (Seq g) (f a) (f b)
    Lab r s   -> flip Lab s H.<$> f r
    End a     -> H.pure (End a)
