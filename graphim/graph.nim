import std/[sets, strformat, tables]

type
  AdjList[T] = TableRef[T, HashSet[T]]

  Graph*[T] = ref object of RootObj
    ## Graph keeps track of two adjacency lists,
    ## successors and predecessors. For undirected graphs,
    ## successors and predecessors reference the same underlying table.
    succ: AdjList[T]
    pred: AdjList[T]

using G: Graph


# Iterators:

iterator nodes*[T](G: Graph[T]): T =
  for node in G.succ.keys: yield node

iterator successors*[T](G: Graph[T], node: T): T =
  for succ in G.succ[node].items: yield succ

iterator predecessors*[T](G: Graph[T], node: T): T =
  for pred in G.pred[node].items: yield pred

iterator outEdges*[T](G: Graph[T], node: T): (T, T) =
  ## Yield each edge in G starting with `node`.
  for succ in G.successors(node): yield (node, succ)

iterator inEdges*[T](G: Graph[T], node: T): (T, T) =
  ## Yield each edge in G ending with `node`.
  for pred in G.predecessors(node): yield (pred, node)

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


# Queries:

proc isDirected*[T](G: Graph[T]): bool =
  G.succ != G.pred  # ref type, comparing identity.

proc contains*[T](G: Graph[T], node: T): bool =
  ## True if G contains node.
  node in G.succ

proc contains*[T](G: Graph[T], edge: (T, T)): bool =
  ## True if G contains edge (u, v).
  let (u, v) = edge
  u in G.succ and v in G.succ[u]

proc outDegree*[T](G; node: T): int =
  ## The out-degree of a node. For undirected graphs,
  ## self-loops are counted twice towards the degree.
  G.succ[node].len + (
    if not G.isDirected and node in G.succ[node]: 1
    else: 0
  )

proc inDegree*[T](G; node: T): int =
  ## The in-degree of a node. For undirected graphs,
  ## self-loops are counted twice towards the degree.
  G.pred[node].len + (
    if not G.isDirected and node in G.pred[node]: 1
    else: 0
  )

proc outDegreeHistogram*(G): CountTable[int] =
  for node in G.nodes: result.inc G.outDegree(node)

proc inDegreeHistogram*(G): CountTable[int] =
  for node in G.nodes: result.inc G.inDegree(node)

proc order*(G): int = G.succ.len
  ## Total nodes in graph.

proc size*(G): int =
  ## Total edges in graph.
  for node in G.nodes: result += G.outDegree node
  if not G.isDirected: result = result div 2

proc `$`*(G): string =
  fmt"Graph on {G.order} nodes with {G.size} edges."


# Construction:

proc newGraph*[T]: Graph[T] =
  ## New undirected graph.
  new result
  result.succ = new AdjList[T]
  result.pred = result.succ

proc newDiGraph*[T]: Graph[T] =
  ## New directed graph.
  new result
  result.succ = new AdjList[T]
  result.pred = new AdjList[T]

proc addNode*[T](G; node: T) =
  if node notin G.succ:
    G.succ[node] = HashSet[T]()

  if node notin G.pred:
    G.pred[node] = HashSet[T]()

proc removeNode*[T](G; node: T) =
  for succ in G.succ[node].items:
    G.pred[succ].excl node

  for pred in G.pred[node].items:
    G.succ[pred].excl node

  G.succ.del node
  G.pred.del node

proc addEdge*[T](G: Graph[T], edge: (T, T)) =
  let (u, v) = edge
  G.addNode u
  G.addNode v

  G.succ[u].incl v
  G.pred[v].incl u

proc removeEdge*[T](G: Graph[T], edge: (T, T)) =
  let (u, v) = edge
  if u in G.succ:
    G.succ[u].excl v

  if v in G.pred:
    G.pred[v].excl u

proc addNodesFrom*[T](G: Graph[T], nodes: openArray[T]) =
  for node in nodes: G.addNode node

proc removeNodesFrom*[T](G: Graph[T], nodes: openArray[T]) =
  for node in nodes: G.removeNode node

proc addEdgesFrom*[T](G: Graph[T], edges: openArray[(T, T)]) =
  for edge in edges: G.addEdge edge

proc removeEdgesFrom*[T](G: Graph[T], edges: openArray[(T, T)]) =
  for edge in edges: G.removeEdge edge

proc newGraphFromNodes*[T](nodes: openArray[T]): Graph[T] =
  result = newGraph[T]()
  result.addNodesFrom(nodes)

proc newDiGraphFromNodes*[T](nodes: openArray[T]): Graph[T] =
  result = newDiGraph[T]()
  result.addNodesFrom(nodes)

proc newGraphFromEdges*[T](edges: openArray[(T, T)]): Graph[T] =
  result = newGraph[T]()
  result.addEdgesFrom(edges)

proc newDiGraphFromEdges*[T](edges: openArray[(T, T)]): Graph[T] =
  result = newDiGraph[T]()
  result.addEdgesFrom(edges)

proc clear*(G) =
  ## Remove all nodes and edges from the graph.
  clear G.succ
  clear G.pred

proc clearEdges*(G) =
  ## Remove all edges from the graph.
  for successors in G.succ.mvalues:
    clear successors

  for predecessors in G.pred.mvalues:
    clear predecessors

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

proc update*[T](G, H: Graph[T]) =
  ## Add all nodes and edges from H to G.
  for node, successors in H.succ:
    G.addNode node
    for succ in successors.items:
      G.addEdge (node, succ)

proc inducedSubgraph*[T](G: Graph[T], nodes: openArray[T]): Graph[T] =
  ## Return a graph with nodes from `nodes` and edges from G that have
  ## both ends in `nodes`.
  let keep = nodes.toHashSet

  result = (
    if G.isDirected: newDiGraphFromNodes nodes
    else: newGraphFromNodes nodes
  )

  for node in nodes:
    if node in G:
      for edge in G.outEdges(node):
        if edge[1] in keep:
          result.addEdge edge


# Mutating:

proc setDirected*(G) =
  ## For undirected graphs, this sets predecessors
  ## to be a separate adjacency list. If the edge (u, v)
  ## was in the undirected graph, the edges (u, v) and
  ## (v, u) will be in the directed graph.
  ##
  ## Does nothing for directed graphs.
  if not G.isDirected: G.pred = copy G.pred

proc setUndirected*(G) =
  ## For directed graphs, this adds all predecessors
  ## to the successors adjacency list. If v was a predecessor
  ## to u in the directed graph, the edge (u, v) will be in the
  ## undirected graph.
  ##
  ## Does nothing for undirected graphs.
  if G.isDirected:
    for node, predecessors in G.pred:
      for pred in predecessors.items:
        G.succ[node].incl pred

    G.pred = G.succ

proc reverse*(G) =
  ## Reverse edges in a directed graph in O(1).
  ## Does nothing for undirected graphs.
  swap(G.succ, G.pred)
