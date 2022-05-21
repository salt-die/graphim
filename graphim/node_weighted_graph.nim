import std/[tables]
import graph


type
  NodeWeightedGraph[T, W] = ref object of Graph[T]
    default_weight: W
    node_weights: Table[T, W]

using G: NodeWeightedGraph

proc addNode*[T, W](G: NodeWeightedGraph[T, W], node: T) =
  G.node_weights[node] = G.default_weight