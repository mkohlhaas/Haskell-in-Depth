{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Graphs where

import Data.Map (Map)
import qualified Data.Map as Map (fromList)

class Graph g where -- g is a type variable for a graph representation, e.g. lists of edges, adjacency matrices, incidence matrices, ...
  type Vertex g -- associated type family: the type of vertices depend on the type of the graph
  data Edge g -- associated data family: the data type for edges will get data constructors in instances.
  src, tgt ∷ Edge g → Vertex g
  outEdges ∷ g → Vertex g → [Edge g]

-- We can define functions that are able to work with any representation.
neighbors ∷ Graph g ⇒ g → Vertex g → [Vertex g]
neighbors g v = map tgt (outEdges g v)

isEdgeLoopy ∷ (Graph g, Eq (Vertex g)) ⇒ g → Edge g → Bool
isEdgeLoopy _ e = src e == tgt e

-- other graph methods
-- ...

-- Implementing graph instances is outside the scope of the book, so I limit myself to the type families' parts of them!

--------------------------------------
-- Representation #1: list of edges --
--------------------------------------
newtype EdgesList = EdgesList [Edge EdgesList] -- Edge representation is not known yet.

-- a graph is a list of edges, a vertex is an Int number and every edge is a pair of vertices
instance Graph EdgesList where
  type Vertex EdgesList = Int
  data Edge EdgesList = MkEdge1 (Vertex EdgesList) (Vertex EdgesList)
  src = undefined
  tgt = undefined
  outEdges = undefined

g1 ∷ EdgesList
g1 = EdgesList [MkEdge1 0 1, MkEdge1 1 0]

------------------------------------------------------------------------
-- Representation #2: lists of adjacent vertices (adjacency matrices) --
------------------------------------------------------------------------
-- map from a vertex to a list of adjacent vertices
newtype VertexMap = VertexMap (Map (Vertex VertexMap) [Vertex VertexMap])

instance Graph VertexMap where
  type Vertex VertexMap = String
  data Edge VertexMap = MkEdge2 Int (Vertex VertexMap) (Vertex VertexMap)
  src = undefined
  tgt = undefined
  outEdges = undefined

g2 ∷ VertexMap
g2 = VertexMap (Map.fromList [("A", ["B"]), ("B", ["A"])])
