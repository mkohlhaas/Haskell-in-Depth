{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Graphs where

import Data.Map (Map)
import qualified Data.Map as Map (fromList)

class Graph g where -- g stands for a graph representation, e.g. lists of edges, adjacency matrices, incidence matrices, ...
  type Vertex g ------ ASSOCIATED TYPE FAMILY: the type of vertices depend on the type of the graph.
  data Edge g -------- ASSOCIATED DATA FAMILY: the data type for edges will get data constructors in instances.
  src, tgt ∷ Edge g → Vertex g
  outEdges ∷ g → Vertex g → [Edge g]

-- >>> :kind Graph
-- Graph ∷ Type → Constraint

-- We can define functions that are able to work with any representation.
neighbors ∷ Graph g ⇒ g → Vertex g → [Vertex g]
neighbors g v = map tgt (outEdges g v)

isEdgeLoopy ∷ (Graph g, Eq (Vertex g)) ⇒ g → Edge g → Bool
isEdgeLoopy _ e = src e == tgt e

-- more graph methods
-- ...

-- Implementing graph instances is outside the scope of the book, so I limit myself to the type families' parts of them!

--------------------------------------
-- Representation #1: list of edges --
--------------------------------------

-- `g` in `Graph g` is a list of edges
newtype EdgesList = EdgesList [Edge EdgesList] -- Edge representation is not known yet.

-- >>> :kind Edge
-- Edge ∷ Type → Type

-- a graph is a list of edges
-- a vertex is an Int number
-- edge is a pair of vertices
instance Graph EdgesList where
  type Vertex EdgesList = Int
  data Edge EdgesList = MkEdge1 (Vertex EdgesList) (Vertex EdgesList)
  outEdges ∷ EdgesList → Vertex EdgesList → [Edge EdgesList]
  src ∷ Edge EdgesList → Vertex EdgesList
  src = undefined
  tgt ∷ Edge EdgesList → Vertex EdgesList
  tgt = undefined
  outEdges = undefined

g1 ∷ EdgesList
g1 = EdgesList [MkEdge1 0 1, MkEdge1 1 0]

-- Implementations are undefined but at least they type check.

-- >>> neighbors g1 0
-- Prelude.undefined

-- >>> isEdgeLoopy g1 (MkEdge1 0 1)
-- Prelude.undefined

-- >>> neighbors g2 "A"
-- Prelude.undefined

-- >>> isEdgeLoopy g2 (MkEdge2 "A" "B")
-- Prelude.undefined

------------------------------------------------------------------------
-- Representation #2: lists of adjacent vertices (adjacency matrices) --
------------------------------------------------------------------------
-- `g` in `Graph g` is a map from a vertex to a list of adjacent vertices
newtype VertexMap = VertexMap (Map (Vertex VertexMap) [Vertex VertexMap])

-- a graph is a map of vertices to list of adjacent vertices
-- a vertex is String
-- edge is a pair of vertices
instance Graph VertexMap where
  type Vertex VertexMap = String
  data Edge VertexMap = MkEdge2 (Vertex VertexMap) (Vertex VertexMap)
  outEdges ∷ VertexMap → Vertex VertexMap → [Edge VertexMap]
  src ∷ Edge VertexMap → Vertex VertexMap
  src = undefined
  tgt ∷ Edge VertexMap → Vertex VertexMap
  tgt = undefined
  outEdges = undefined

g2 ∷ VertexMap
g2 = VertexMap (Map.fromList [("A", ["B"]), ("B", ["A"])])
