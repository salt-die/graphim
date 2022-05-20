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
  if not G.isDirected: G.pred = copy G.pred

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

proc addEdge*[T](G: Graph[T], edge: (T, T)) =
  let (u, v) = edge
  G.addNode u
  G.addNode v

  G.succ[][u].incl v
  G.pred[][v].incl u

proc removeEdge*[T](G: Graph[T], edge: (T, T)) =
  let (u, v) = edge
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

proc contains*[T](G: Graph[T], edge: (T, T)): bool =
  ## True if G contains edge (u, v).
  let (u, v) = edge
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
  ## The out-degree of a node. For undirected graphs,
  ## self-loops are counted twice towards the degree.
  G.succ[][node].len + (
    if not G.isDirected and node in G.succ[][node]: 1
    else: 0
  )

proc outDegreeHistogram*[T](G: Graph[T]): CountTable[int] =
  for node in G.succ[].keys:
    result.inc G.outDegree(node)

proc inDegree*[T](G: Graph, node: T): int =
  ## The in-degree of a node. For undirected graphs,
  ## self-loops are counted twice towards the degree.
  G.pred[][node].len + (
    if not G.isDirected and node in G.pred[][node]: 1
    else: 0
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
  ## Yield each edge in G starting with `node`.
  for succ in G.successors(node):
    yield (node, succ)

iterator inEdges*[T](G: Graph[T], node: T): (T, T) =
  ## Yield each edge in G ending with `node`.
  for pred in G.predecessors(node):
    yield (pred, node)

iterator edges*[T](G: Graph[T]): (T, T) =
  ## Yield each edge in G. For undirected graphs,
  ## only one of (u, v) or (v, u) will be yielded.
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

proc inducedSubgraph*[T](G: Graph[T], nodes: openArray[T]): Graph[T] =
  ## Return a graph with nodes from `nodes` and edges from G that have
  ## both ends in `nodes`.
  let keep = nodes.toHashSet

  result = copy G

  for node in nodes:
    result.addNode node

  for node in G.nodes:
    if node notin keep:
      result.removeNode node

proc addNodesFrom*[T](G: Graph[T], nodes: openArray[T]) =
  for node in nodes:
    G.addNode node

proc removeNodesFrom*[T](G: Graph[T], nodes: openArray[T]) =
  for node in nodes:
    G.removeNode node

proc addEdgesFrom*[T](G: Graph[T], edges: openArray[(T, T)]) =
  for edge in edges:
    G.addEdge edge

proc removeEdgesFrom*[T](G: Graph[T], edges: openArray[(T, T)]) =
  for edge in edges:
    G.removeEdge edge

proc newGraphFromNodes*[T](nodes: openArray[T]): Graph[T] =
  result = newGraph[T]()
  result.add_nodes_from(nodes)

proc newDiGraphFromNodes*[T](nodes: openArray[T]): Graph[T] =
  result = newDiGraph[T]()
  result.add_nodes_from(nodes)

proc newGraphFromEdges*[T](edges: openArray[(T, T)]): Graph[T] =
  result = newGraph[T]()
  result.addEdgesFrom(edges)

proc newDiGraphFromEdges*[T](edges: openArray[(T, T)]): Graph[T] =
  result = newDiGraph[T]()
  result.addEdgesFrom(edges)

proc `$`*(G: Graph): string =
  fmt"Graph on {G.order} nodes with {G.size} edges."

when isMainModule:
  import std/[random, sugar]

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
    echo "out degree histogram: ", outDegreeHistogram G
    echo "in degree histogram: ", inDegreeHistogram G

  var G = newDiGraphFromEdges(
    collect(
      for _ in 0..<20:
        (rand 9, rand 9)
    )
  )

  display G
  display G.inducedSubgraph([0, 1, 2, 3, 4])
