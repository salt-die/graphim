import std/[sets, strformat, tables]

type
  AdjList[T] = ref Table[T, HashSet[T]]

  Graph*[T] = ref object
    ## Graph keeps track of two adjacency lists:
    ## successors and predecessors. For undirected graphs,
    ## successors and predecessors reference the same underlying table.
    succ: AdjList[T]
    pred: AdjList[T]

proc newGraph*[T]: Graph[T] =
  new result
  result.succ = new AdjList[T]
  result.pred = result.succ

proc newDiGraph*[T]: Graph[T] =
  new result
  result.succ = new AdjList[T]
  result.pred = new AdjList[T]

proc isDirected*[T](G: Graph[T]): bool =
  G.succ != G.pred  # ref type, comparing identity.

proc copy[T](adjList: AdjList[T]): AdjList[T] =
  new result
  for k, v in adjList:
    result[][k] = v

proc setDirected*[T](G: Graph[T]) =
  if not G.isDirected:
    G.pred = copy G.pred

proc setUndirected*[T](G: Graph[T]) =
  if G.isDirected:
    for node, neighbors in G.pred:
      for neighbor in neighbors:
        G.succ[][node].incl neighbor

    G.pred = G.succ

proc reverse*[T](G: Graph[T]) =
  swap(G.succ, G.pred)

proc addNode*[T](G: Graph, node: T) =
  if node notin G.succ[]:
    G.succ[][node] = HashSet[T]()

  if node notin G.pred[]:
    G.pred[][node] = HashSet[T]()

proc addEdge*[T](G: Graph[T]; u, v: T) =
  G.succ[][u].incl v

  if u notin G.pred[]:
    G.pred[][u] = HashSet[T]()

  G.pred[][v].incl u

  if v notin G.succ[]:
    G.succ[][v] = HashSet[T]()

iterator successors*[T](G: Graph[T], node: T): T =
  for succ in G.succ[][node]:
    yield succ

iterator predecessors*[T](G: Graph[T], node: T): T =
  for pred in G.pred[][node]:
    yield pred

proc order*(G: Graph): int = G.succ[].len
  ## Total nodes in graph.

proc outDegree*[T](G: Graph, node: T): int =
  G.succ[][node].len + (
    if not G.isDirected and node in G.succ[][node]:
      1
    else:
      0
  )

proc inDegree*[T](G: Graph, node: T): int =
  G.pred[][node].len + (
    if not G.isDirected and node in G.pred[][node]:
      1
    else:
      0
  )

iterator nodes*[T](G: Graph[T]): T =
  for node in G.succ[].keys:
    yield node

proc size*(G: Graph): int =
  ## Total edges in graph.
  for node in G.nodes:
    result += G.outDegree node

  if not G.isDirected:
    result = result div 2

iterator outEdges*[T](G: Graph[T], node: T): (T, T) =
  for succ in G.successors(node):
    yield (node, succ)

iterator inEdges*[T](G: Graph[T], node: T): (T, T) =
  for pred in G.predecessors(node):
    yield (pred, node)

proc `$`*(G: Graph): string =
  fmt"Graph on {G.order} nodes with {G.size} edges."

when isMainModule:
  import std/random

  proc repr(G: Graph): string =
    fmt"""
Graph(
  succ:
    {G.succ[]},
  pred:
    {G.pred[]},
)"""

  proc display(G: Graph) =
    echo repr(G)
    echo G

  var G = newDiGraph[int]()
  for i in 0..<10:
    G.addNode i

  for _ in 0..<20:
    let
      u = rand(9)
      v = rand(9)
    G.addEdge(u, v)

  display G

  reverse G
  display G

  setUndirected G
  display G
