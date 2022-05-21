import std/[tables]
import graph


type
  EdgeWeightedGraph[T, W] = ref object of Graph[T]
    default_weight: W
    outWeights: TableRef[(T, T), W]
    inWeights: TableRef[(T, T), W]

using G: EdgeWeightedGraph


# Queries:
proc edgeWeight*[T, W](G: EdgeWeightedGraph[T, W], edge: (T, T)): W =
  G.outWeights[edge]


# Construction:

proc addEdge*[T, W](G: EdgeWeightedGraph[T, W], edge: (T, T)) =
  let (u, v) = edge
  G.addNode u
  G.addNode v

  G.succ[u].incl v
  G.pred[v].incl u

  G.outWeights[edge] = G.default_weight
  G.inWeights[(v, u)] = G.default_weight

proc addEdge*[T, W](G: EdgeWeightedGraph[T, W], edge: (T, T), weight: W) =
  let (u, v) = edge
  G.addNode u
  G.addNode v

  G.succ[u].incl v
  G.pred[v].incl u

  G.outWeights[edge] = weight
  G.inWeights[(v, u)] = weight
