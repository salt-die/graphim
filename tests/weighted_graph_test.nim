import std/[random, sequtils, strformat, strutils, sugar, tables]
import ../graphim/weighted_graph

proc repr(G: WeightedGraph): string =
  ## This representation is only suitable for small graphs.
  let
    outEdges = collect(
      for node in G.nodes:
        let s = G.successors(node).toSeq.join(", ")
        fmt"    {node}: {s}"
    ).join("\n")

    inEdges = collect(
      for node in G.nodes:
        let s = G.predecessors(node).toSeq.join(", ")
        fmt"    {node}: {s}"
    ).join("\n")

  fmt"""
Graph:
  succ:
{outEdges}
  pred:
{inEdges}
"""

proc showHistogram(histogram: CountTable[int]) =
  echo collect(for k, v in histogram: fmt"  {k}: {v}").join("\n")

proc display(G: WeightedGraph) =
  echo G
  echo repr G
  echo "Out Histogram: "
  G.outDegreeHistogram.showHistogram
  echo "In Histogram: "
  G.inDegreeHistogram.showHistogram

var G = newWeightedDiGraphFromEdges(
  collect(
    for _ in 0..<20:
      (rand 9, rand 9, rand 100)
  )
)

display G
display G.inducedSubgraph([0, 1, 2, 3, 4])
reverse G
display G
setUndirected G
display G
var H = copy G
clear G
display G
G.update H
display G