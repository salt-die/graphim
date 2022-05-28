## Undirected adjacency set implementation.
import std/[tables, sets]
import concepts

type EdgeTableSet*[T: Hashable] = Table[T, HashSet[T]]

proc contains*[T](edges: EdgeTableSet[T], edge: (T, T)): bool =
  let (u, v) = edge
  u in edges and v in edges[u]

iterator successors*[T](edges: EdgeTableSet[T], node: T): T =
  for neighbor in edges[node].items: yield neighbor

iterator edges*[T](edges: EdgeTableSet[T]): (T, T) =
  var seen = HashSet[(T, T)]()
  for node, neighbors in edges.pairs:
    for neighbor in neighbors.items:
      if (neighbor, node) notin seen:
        seen.incl (node, neighbor)
        yield (node, neighbor)

proc isDirected*[T](edges: EdgeTableSet[T]): bool = false

proc outDegree*[T](edges: EdgeTableSet[T], node: T): int =
  edges[node].len + (
    if node in edges[node]: 1
    else: 0
  )

proc size*[T](edges: EdgeTableSet[T]): int =
  for node in edges.keys:
    result += edges.outDegree node

  result = result div 2

proc addNode*[T](edges: var EdgeTableSet[T], node: T) =
  if node notin edges:
    edges[node] = HashSet[T]()

proc removeNode*[T](edges: var EdgeTableSet[T], node: T) =
  for succ in edges.successors(node):
    edges[succ].excl node
  edges.del node

proc addEdge*[T](edges: var EdgeTableSet[T], edge: (T, T)) =
  let (u, v) = edge
  edges.addNode u
  edges.addNode v
  edges[u].incl v
  edges[v].incl u

proc removeEdge*[T](edges: var EdgeTableSet[T], edge: (T, T)) =
  let (u, v) = edge
  if u in edges and v in edges:
    edges[u].excl v
    edges[v].excl u

proc clearEdges*(edges: var EdgeTableSet) = clear edges