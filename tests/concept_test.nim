import std/sugar
import ../graphim/[concepts, node_set, edge_table_set]

makeGraphImpl(int, NodeSet, EdgeTableSet)

proc repr(G: Graph): string =
  ## This representation is only suitable for small graphs.
  let outEdges = collect(
      for node in G.nodes:
        let s = G.successors(node).toSeq.join(", ")
        fmt"  {node}: {{{s}}}"
    ).join("\n")

  fmt"""
Graph:
{outEdges}
"""

proc display(G: Graph) =
  echo repr G

var G = newGraph()

G.addEdgesFrom [
  (0, 1),
  (1, 2),
  (2, 3),
  (3, 4),
  (4, 0),
  (0, 5),
  (1, 6),
  (2, 7),
  (3, 8),
  (4, 9),
  (5, 7),
  (6, 8),
  (7, 9),
  (8, 5),
  (9, 6),
]  # Peterson graph.
display G

echo G[0].neighbors
for node in G[0].successors:
  echo node
