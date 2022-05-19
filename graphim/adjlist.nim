import std/[sets, strformat, tables]

type
  AdjList[T] = TableRef[T, HashSet[T]]

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
  result[] = adjList[]

proc copy*[T](G: Graph[T]): Graph[T] =
  new result
  result.succ = copy G.succ

  if not G.isDirected:
    result.pred = result.succ
  else:
    result.pred = copy G.pred

proc setDirected*[T](G: Graph[T]) =
  ## For undirected graphs, this sets predecessors
  ## to be a separate adjacency list. If the edge (u, v)
  ## was in the undirected graph, the edges (u, v) and
  ## (v, u) will be in the directed graph.
  ##
  ## Does nothing for directed graphs.
  if not G.isDirected:
    G.pred = copy G.pred

proc setUndirected*[T](G: Graph[T]) =
  ## For directed graphs, this adds all predecessors
  ## to the successors adjacency list. If v was a predecessor
  ## to u in the directed graph, the edge (u, v) will be in the
  ## undirected graph.
  ##
  ## Does nothing for undirected graphs.
  if G.isDirected:
    for node, neighbors in G.pred:
      for neighbor in neighbors:
        G.succ[][node].incl neighbor

    G.pred = G.succ

proc reverse*[T](G: Graph[T]) =
  ## Reverse edges in a directed graph in O(1).
  ## Does nothing for undirected graphs.
  swap(G.succ, G.pred)

proc addNode*[T](G: Graph, node: T) =
  if node notin G.succ[]:
    G.succ[][node] = HashSet[T]()

  if node notin G.pred[]:
    G.pred[][node] = HashSet[T]()

proc removeNode*[T](G: Graph, node: T) =
  for succ in G.succ[][node]:
    G.pred[][succ].excl node

  for pred in G.pred[][node]:
    G.succ[][pred].excl node

  G.succ[].del node
  G.pred[].del node

proc addEdge*[T](G: Graph[T], u, v: T) =
  G.addNode u
  G.addNode v

  G.succ[][u].incl v
  G.pred[][v].incl u

proc removeEdge*[T](G: Graph[T], u, v: T) =
  if u in G.succ[]:
    G.succ[][u].excl v
    G.pred[][v].excl u

proc clear*[T](G: Graph[T]) =
  ## Remove all nodes and edges from a graph.
  clear G.succ[]
  clear G.pred[]

proc clearEdges*[T](G: Graph[T]) =
  ## Remove all edges from a graph.
  for successors in G.succ[].mvalues:
    clear successors

  for predecessors in G.pred[].mvalues:
    clear predecessors

proc update*[T](G, H: Graph[T]) =
  ## Add all nodes and edges from H to G.
  for node, successors in H.succ[]:
    for succ in successors:
      G.addEdge node, succ

proc contains*[T](G: Graph[T], node: T): bool =
  ## True if G contains node.
  node in G.succ[]

proc contains*[T](G: Graph[T], u, v: T): bool =
  ## True if G contains edge (u, v).
  u in G.succ[] and v in G.succ[][u]

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

proc outDegreeHistogram*[T](G: Graph[T]): CountTable[int] =
  for node in G.succ[].keys:
    result.inc G.outDegree(node)

proc inDegree*[T](G: Graph, node: T): int =
  G.pred[][node].len + (
    if not G.isDirected and node in G.pred[][node]:
      1
    else:
      0
  )

proc inDegreeHistogram*[T](G: Graph[T]): CountTable[int] =
  for node in G.pred[].keys:
    result.inc G.inDegree(node)

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

iterator outEdges*[T](G: Graph[T]): (T, T) =
  if G.isDirected:
    for node in G.nodes:
      for edge in G.outEdges(node):
        yield edge
  else:
    var seen = HashSet[(T, T)]()
    for node in G.nodes:
      for u, v in G.outEdges(node):
        if (v, u) notin seen:
          seen.incl (u, v)
          yield (u, v)

iterator inEdges*[T](G: Graph[T]): (T, T) =
  if G.isDirected:
    for node in G.nodes:
      for edge in G.inEdges(node):
        yield edge
  else:
    var seen = HashSet[(T, T)]()
    for node in G.nodes:
      for u, v in G.inEdges(node):
        if (v, u) notin seen:
          seen.incl (u, v)
          yield (u, v)

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
    echo repr G
    echo G
    echo outDegreeHistogram G
    echo inDegreeHistogram G

  var G = newDiGraph[int]()
  for i in 0..<10:
    G.addNode i

  for _ in 0..<20:
    let
      u = rand 9
      v = rand 9
    G.addEdge(u, v)

  display G
  setUndirected G
  display G
