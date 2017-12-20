{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, RecordWildCards #-}
module Data.Graph where

data Graph a = Graph { graphName :: Maybe String, graphVertices :: [Vertex a], graphEdges :: [Edge] }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Vertex a = Vertex { vertexIdentifier :: Int, vertexValue :: Maybe a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Edge = Edge { edgeFrom :: Int, edgeTo :: Int }
  deriving (Eq, Ord, Show)

dot :: Show a => Graph a -> String
dot g = dotGraph g ""

dotGraph :: Show a => Graph a -> ShowS
dotGraph Graph{..} = showString "digraph " . maybe id showString graphName . showBraces True (showChar '\n' . foldr ((.) . dotVertex) id graphVertices . foldr ((.) . dotEdge) id graphEdges)

dotVertex :: Show a => Vertex a -> ShowS
dotVertex Vertex{..} = showChar '\t' . shows vertexIdentifier . showBrackets True (maybe id ((showString "label = " .) . shows) vertexValue) . showString ";\n"

dotEdge :: Edge -> ShowS
dotEdge Edge{..} = showChar '\t' . shows edgeFrom . showString " -> " . shows edgeTo . showString ";\n"

showBraces :: Bool -> ShowS -> ShowS
showBraces False s = s
showBraces True s = showChar '{' . s . showChar '}'

showBrackets :: Bool -> ShowS -> ShowS
showBrackets False s = s
showBrackets True s = showChar '[' . s . showChar ']'
