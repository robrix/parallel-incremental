{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, RecordWildCards #-}
module Data.Graph
( Graph(..)
, Vertex(..)
, Edge(..)
, dot
, Semigroup(..)
, Semiring(..)
) where

import Data.Function (on)
import Data.List (union, unionBy)
import Data.Semiring

data Graph a = Graph { graphVertices :: [Vertex a], graphEdges :: [Edge] }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Vertex a = Vertex { vertexIdentifier :: Int, vertexValue :: Maybe a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Edge = Edge { edgeFrom :: Int, edgeTo :: Int }
  deriving (Eq, Ord, Show)

dot :: Show a => Graph a -> String
dot g = dotGraph g ""

dotGraph :: Show a => Graph a -> ShowS
dotGraph Graph{..} = showString "digraph " . showBraces True (showChar '\n' . foldr ((.) . dotVertex) id graphVertices . foldr ((.) . dotEdge) id graphEdges)

dotVertex :: Show a => Vertex a -> ShowS
dotVertex Vertex{..} = showChar '\t' . shows vertexIdentifier . showBrackets True (showString "label = " . maybe (shows "") shows vertexValue) . showString ";\n"

dotEdge :: Edge -> ShowS
dotEdge Edge{..} = showChar '\t' . shows edgeFrom . showString " -> " . shows edgeTo . showString ";\n"

showBraces :: Bool -> ShowS -> ShowS
showBraces False s = s
showBraces True s = showChar '{' . s . showChar '}'

showBrackets :: Bool -> ShowS -> ShowS
showBrackets False s = s
showBrackets True s = showChar '[' . s . showChar ']'


instance Semigroup (Graph a) where
  Graph v1 e1 <> Graph v2 e2 = Graph (unionBy ((==) `on` vertexIdentifier) v1 v2) (e1 `union` e2)

instance Monoid (Graph a) where
  mempty = Graph [] []
  mappend = (<>)

instance Semiring (Graph a) where
  Graph v1 e1 >< Graph v2 e2 = Graph (unionBy ((==) `on` vertexIdentifier) v1 v2) (e1 `union` e2 `union` ((Edge `on` vertexIdentifier) <$> v1 <*> v2))
