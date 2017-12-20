module Data.Graph where

data Graph a = Graph { graphName :: Maybe String, graphVertices :: [Vertex a], graphEdges :: [Edge] }

data Vertex a = Vertex { vertexIdentifier :: Int, vertexValue :: Maybe a }

data Edge = Edge { edgeFrom :: Int, edgeTo :: Int }
