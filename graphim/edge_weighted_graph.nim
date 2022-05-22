import std/[sets, strformat, tables]

type
  EdgeWeightedAdjList[T, W] = TableRef[T, Table[T, W]]

  EdgeWeightedGraph*[T, W] = ref object
    ## EdgeWeightedGraph keeps track of two adjacency lists,
    ## successors and predecessors. For undirected graphs,
    ## successors and predecessors reference the same underlying table.
    succ: EdgeWeightedAdjList[T, W]
    pred: EdgeWeightedAdjList[T, W]

using G: EdgeWeightedGraph

# Iterators:

iterator nodes*[T, W](G: EdgeWeightedGraph[T, W]): T =
  for node in G.succ.keys: yield node

iterator successors*[T, W](G: EdgeWeightedGraph[T, W], node: T): T =
  for succ in G.succ[node].keys:  yield succ

iterator predecessors*[T, W](G: EdgeWeightedGraph[T, W], node: T): T =
  for pred in G.pred[node].keys: yield pred

iterator outEdges*[T, W](G: EdgeWeightedGraph[T, W], node: T): (T, T, W) =
  ## Yield each edge in G starting with `node`.
  for succ, weight in G.succ[node].pairs: yield (node, succ, weight)

iterator inEdges*[T, W](G: EdgeWeightedGraph[T, W], node: T): (T, T, W) =
  ## Yield each edge in G ending with `node`.
  for pred, weight in G.pred[node].pairs: yield (pred, node, weight)

iterator edges*[T, W](G: EdgeWeightedGraph[T, W]): (T, T, W) =
  ## Yield each edge in G. For undirected graphs,
  ## only one of (u, v, w) or (v, u, w) will be yielded.
  if G.isDirected:
    for node in G.nodes:
      for edge in G.outEdges(node):
        yield edge
  else:
    var seen = HashSet[(T, T)]()
    for node in G.nodes:
      for u, v, w in G.outEdges(node):
        if (v, u) notin seen:
          seen.incl (u, v)
          yield (u, v, w)


# Queries:

proc isDirected*[T, W](G: EdgeWeightedGraph[T, W]): bool =
  G.succ != G.pred  # ref type, comparing identity.

proc contains*[T, W](G: EdgeWeightedGraph[T, W], node: T): bool =
  ## True if G contains node.
  node in G.succ

proc contains*[T, W](G: EdgeWeightedGraph[T, W], edge: (T, T)): bool =
  ## True if G contains edge (u, v).
  let (u, v) = edge
  u in G.succ and v in G.succ[u]

proc outDegree*[T, W](G: EdgeWeightedGraph[T, W], node: T): int =
  ## The out-degree of a node. For undirected graphs,
  ## self-loops are counted twice towards the degree.
  G.succ[node].len + (
    if not G.isDirected and node in G.succ[node]: 1
    else: 0
  )

proc inDegree*[T, W](G: EdgeWeightedGraph[T, W], node: T): int =
  ## The in-degree of a node. For undirected graphs,
  ## self-loops are counted twice towards the degree.
  G.pred[node].len + (
    if not G.isDirected and node in G.succ[node]: 1
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

proc weight*[T, W](G: EdgeWeightedGraph[T, W], edge: (T, T)): W =
  let (u, v) = edge
  G.succ[u][v]


# Construction:

proc newEdgeWeightedGraph*[T, W]: EdgeWeightedGraph[T, W] =
  ## New undirected weighted graph.
  new result
  result.succ = new EdgeWeightedAdjList[T, W]
  result.pred = result.succ

proc newEdgeWeightedDiGraph*[T, W]: EdgeWeightedGraph[T, W] =
  ## New directed weighted graph.
  new result
  result.succ = new EdgeWeightedAdjList[T, W]
  result.pred = new EdgeWeightedAdjList[T, W]

proc addNode*[T, W](G: EdgeWeightedGraph[T, W], node: T) =
  if node notin G.succ:
    G.succ[node] = Table[T, W]()

  if node notin G.pred:
    G.pred[node] = Table[T, W]()

proc removeNode*[T, W](G: EdgeWeightedGraph[T, W], node: T) =
  for succ in G.succ[node].keys:
    G.pred[succ].del node

  for pred in G.pred[node].keys:
    G.succ[pred].del node

  G.succ.del node
  G.pred.del node

proc addEdge*[T, W](G: EdgeWeightedGraph[T, W], edge: (T, T, W)) =
  let (u, v, w) = edge
  G.addNode u
  G.addNode v

  G.succ[u][v] = w
  G.pred[v][u] = w

proc removeEdge*[T, W](G: EdgeWeightedGraph[T, W], edge: (T, T)) =
  let (u, v) = edge
  if u in G.succ:
    G.succ[u].del v

  if v in G.pred:
    G.pred[v].del u

proc addNodesFrom*[T, W](G: EdgeWeightedGraph[T, W], nodes: openArray[T]) =
  for node in nodes: G.addNode node

proc removeNodesFrom*[T, W](G: EdgeWeightedGraph[T, W], nodes: openArray[T]) =
  for node in nodes: G.removeNode node

proc addEdgesFrom*[T, W](G: EdgeWeightedGraph[T, W], edges: openArray[(T, T, W)]) =
  for edge in edges: G.addEdge edge

proc removeEdgesFrom*[T, W](G: EdgeWeightedGraph[T, W], edges: openArray[(T, T, W)]) =
  for edge in edges: G.removeEdge edge

proc newEdgeWeightedGraphFromNodes*[T](nodes: openArray[T], W: typedesc): EdgeWeightedGraph[T, W] =
  result = newEdgeWeightedGraph[T, W]()
  result.addNodesFrom nodes

proc newEdgeWeightedDiGraphFromNodes*[T](nodes: openArray[T], W: typedesc): EdgeWeightedGraph[T, W] =
  result = newEdgeWeightedDiGraph[T, W]()
  result.addNodesFrom nodes

proc newEdgeWeightedGraphFromEdges*[T, W](edges: openArray[(T, T, W)]): EdgeWeightedGraph[T, W] =
  result = newEdgeWeightedGraph[T, W]()
  result.addEdgesFrom edges

proc newEdgeWeightedDiGraphFromEdges*[T, W](edges: openArray[(T, T, W)]): EdgeWeightedGraph[T, W] =
  result = newEdgeWeightedDiGraph[T, W]()
  result.addEdgesFrom edges

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

proc copy[T, W](adjList: EdgeWeightedAdjList[T, W]): EdgeWeightedAdjList[T, W] =
  new result
  result[] = adjList[]

proc copy*[T, W](G: EdgeWeightedGraph[T, W]): EdgeWeightedGraph[T, W] =
  new result
  result.succ = copy G.succ

  if not G.isDirected:
    result.pred = result.succ
  else:
    result.pred = copy G.pred

proc update*[T, W](G, H: EdgeWeightedGraph[T, W]) =
  ## Add all nodes and edges from H to G.
  for node, successors in H.succ:
    G.addNode node
    for succ, weight in successors:
      G.addEdge (node, succ, weight)

proc inducedSubgraph*[T, W](G: EdgeWeightedGraph[T, W], nodes: openArray[T]): EdgeWeightedGraph[T, W] =
  ## Return a graph with nodes from `nodes` and edges from G that have
  ## both ends in `nodes`.
  let keep = nodes.toHashSet

  result = (
    if G.isDirected: newEdgeWeightedDiGraphFromNodes(nodes, W)
    else: newEdgeWeightedGraphFromNodes(nodes, W)
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
      for pred, weight in predecessors:
        G.succ[node][pred] = weight

    G.pred = G.succ

proc reverse*(G) =
  ## Reverse edges in a directed graph in O(1).
  ## Does nothing for undirected graphs.
  swap(G.succ, G.pred)
