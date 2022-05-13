import std/[sets, strformat, tables]

type
  GraphKind* = enum
    gkGraph = "Graph"
    gkDiGraph = "Digraph"
    gkMultiGraph = "MultiGraph"
    gkMultiDiGraph = "MultiDiGraph"

  Adj[T, W] = Table[T, Table[T, W]]
  MultiAdj[T, W] = Table[T, Table[T, Table[int, W]]]

  Graph*[T; W: SomeNumber] = ref object
    nodes: Table[T, W]
    case kind: GraphKind
    of gkGraph, gkDiGraph: adj: Adj[T, W]
    of gkMultiGraph, gkMultiDiGraph:
      multiadj: MultiAdj[T, W]
      uid: int

using graph: Graph

iterator nodes*[T; W: SomeNumber](graph: Graph[T, W]): T =
  for node, _ in graph.nodes: yield node

iterator node_weights*[T; W: SomeNumber](graph: Graph[T, W]): (T, W) =
  for node, weight in graph.nodes: yield (node, weight)

iterator edges*[T; W: SomeNumber](graph: Graph[T, W]): (T, T) =
  case graph.kind
  of gkGraph:
    var seen = HashSet[(T, T)]()
    for node, neighbors in graph.adj:
      for neighbor, _ in neighbors:
        if (node, neighbor) notin seen:
          yield (node, neighbor)
          seen.incl (neighbor, node)
  of gkDiGraph:
    for node, neighbors in graph.adj:
      for neighbor, _ in neighbors:
        yield (node, neighbor)
  of gkMultiGraph:
    var seen = HashSet[(T, T, int)]()
    for node, neighbors in graph.multiadj:
      for neighbor, unique in neighbors:
        for uid, _ in unique:
          if (node, neighbor, uid) notin seen:
            yield (node, neighbor)
            seen.incl (neighbor, node, uid)
  of gkMultiDiGraph:
    for node, neighbors in graph.multiadj:
      for neighbor, unique in neighbors:
        for _ in 0..<unique.len:
          yield (node, neighbor)

iterator edge_weights*[T; W: SomeNumber](graph: Graph[T, W]): (T, T, W) =
  case graph.kind
  of gkGraph:
    var seen = HashSet[(T, T)]()
    for node, neighbors in graph.adj:
      for neighbor, weight in neighbors:
        if (node, neighbor) notin seen:
          yield (node, neighbor, weight)
          seen.incl (neighbor, node)
  of gkDiGraph:
    for node, neighbors in graph.adj:
      for neighbor, weight in neighbors:
        yield (node, neighbor, weight)
  of gkMultiGraph:
    var seen = HashSet[(T, T, int)]()
    for node, neighbors in graph.multiadj:
      for neighbor, unique in neighbors:
        for uid, weight in unique:
          if (node, neighbor, uid) notin seen:
            yield (node, neighbor, weight)
            seen.incl (neighbor, node, uid)
  of gkMultiDiGraph:
    for node, neighbors in graph.multiadj:
      for neighbor, unique in neighbors:
        for _, weight in unique:
          yield (node, neighbor, weight)

proc add_node*[T; W: SomeNumber](graph; node: T, weight: W = 1) =
  graph.nodes[node] = weight
  case graph.kind
  of gkGraph, gkDiGraph:
    if node notin graph.adj:
      graph.adj[node] = Table[T, W]()
  of gkMultiGraph, gkMultiDiGraph:
    if node notin graph.multiadj:
      graph.multiadj[node] = Table[T, Table[int, W]]()

proc contains*[T](graph; node: T): bool =
  node in graph.nodes

proc add_edge*[T; W: SomeNumber](graph; u, v: T, weight: W = 1) =
  if u notin graph: graph.add_node(u, 1.W)
  if v notin graph: graph.add_node(v, 1.W)
  case graph.kind
  of gkGraph:
    graph.adj[u][v] = weight
    graph.adj[v][u] = weight
  of gkDiGraph:
    graph.adj[u][v] = weight
  of gkMultiGraph:
    if v notin graph.multiadj[u]:
      graph.multiadj[u][v] = Table[int, W]()
    if u notin graph.multiadj[v]:
      graph.multiadj[v][u] = Table[int, W]()

    graph.multiadj[u][v][graph.uid] = weight
    graph.multiadj[v][u][graph.uid] = weight
    inc graph.uid
  of gkMultiDiGraph:
    if v notin graph.multiadj[u]:
      graph.multiadj[u][v] = Table[int, W]()

    graph.multiadj[u][v][graph.uid] = weight
    inc graph.uid

proc add_nodes_from*[T](graph; graph_data: openArray[T]) =
  for node in graph_data: graph.add_node(node)

proc add_nodes_from*[T](graph; graph_data: seq[T]) =
  for node in graph_data: graph.add_node(node)

proc add_nodes_from*[T](graph; graph_data: iterator: T) =
  for node in graph_data: graph.add_node(node)

proc add_nodes_from*[T; W: SomeNumber](graph; graph_data: openArray[(T, W)]) =
  for (u, w) in graph_data: graph.add_node(u, w)

proc add_nodes_from*[T; W: SomeNumber](graph; graph_data: seq[(T, W)]) =
  for (u, w) in graph_data: graph.add_node(u, w)

proc add_nodes_from*[T; W: SomeNumber](graph; graph_data: iterator: (T, W)) =
  for (u, w) in graph_data: graph.add_node(u, w)

proc add_edges_from*[T](graph; graph_data: openArray[(T, T)]) =
  for (u, v) in graph_data: graph.add_edge(u, v)

proc add_edges_from*[T](graph; graph_data: seq[(T, T)]) =
  for (u, v) in graph_data: graph.add_edge(u, v)

proc add_edges_from*[T](graph; graph_data: iterator: (T, T)) =
  for (u, v) in graph_data: graph.add_edge(u, v)

proc add_edges_from*[T; W: SomeNumber](graph; graph_data: openArray[(T, T, W)]) =
  for (u, v, w) in graph_data: graph.add_edge(u, v, w)

proc add_edges_from*[T; W: SomeNumber](graph; graph_data: seq[(T, T, W)]) =
  for (u, v, w) in graph_data: graph.add_edge(u, v, w)

proc add_edges_from*[T; W: SomeNumber](graph; graph_data: iterator: (T, T, W)) =
  for (u, v, w) in graph_data: graph.add_edge(u, v, w)

proc initGraph*[T; W: SomeNumber](kind: GraphKind): Graph[T, W] =
  case kind:
  of gkGraph, gkDiGraph:
    result = Graph[T, W](kind: kind, nodes: Table[T, W](), adj: Adj[T, W]())
  of gkMultiGraph, gkMultiDiGraph:
    result = Graph[T, W](kind: kind, nodes: Table[T, W](), multiadj: MultiAdj[T, W]())

proc from_nodes*[T](kind: GraphKind, graph_data: openArray[T]): Graph[T, int] =
  result = initGraph[T, int] kind
  result.add_nodes_from(graph_data)

proc from_nodes*[T](kind: GraphKind, graph_data: seq[T]): Graph[T, int] =
  result = initGraph[T, int] kind
  result.add_nodes_from(graph_data)

proc from_nodes*[T](kind: GraphKind, graph_data: iterator: T): Graph[T, int] =
  result = initGraph[T, int] kind
  result.add_nodes_from(graph_data)

proc from_nodes*[T; W: SomeNumber](kind: GraphKind, graph_data: openArray[(T, W)]): Graph[T, W] =
  result = initGraph[T, W] kind
  result.add_nodes_from(graph_data)

proc from_nodes*[T; W: SomeNumber](kind: GraphKind, graph_data: seq[(T, W)]): Graph[T, W] =
  result = initGraph[T, W] kind
  result.add_nodes_from(graph_data)

proc from_nodes*[T; W: SomeNumber](kind: GraphKind, graph_data: iterator: (T, W)): Graph[T, W] =
  result = initGraph[T, W] kind
  result.add_nodes_from(graph_data)

proc from_edges*[T](kind: GraphKind, graph_data: openArray[(T, T)]): Graph[T, int] =
  result = initGraph[T, int] kind
  result.add_edges_from(graph_data)

proc from_edges*[T](kind: GraphKind, graph_data: seq[(T, T)]): Graph[T, int] =
  result = initGraph[T, int] kind
  result.add_edges_from(graph_data)

proc from_edges*[T](kind: GraphKind, graph_data: iterator: (T, T)): Graph[T, int] =
  result = initGraph[T, int] kind
  result.add_edges_from(graph_data)

proc from_edges*[T; W: SomeNumber](kind: GraphKind, graph_data: openArray[(T, T, W)]): Graph[T, W] =
  result = initGraph[T, W] kind
  result.add_edges_from(graph_data)

proc from_edges*[T; W: SomeNumber](kind: GraphKind, graph_data: seq[(T, T, W)]): Graph[T, W] =
  result = initGraph[T, W] kind
  result.add_edges_from(graph_data)

proc from_edges*[T; W: SomeNumber](kind: GraphKind, graph_data: iterator: (T, T, W)): Graph[T, W] =
  result = initGraph[T, W] kind
  result.add_edges_from(graph_data)

proc order*(graph): int = graph.nodes.len

proc degree*[T](graph; node: T): int =
  case graph.kind
  of gkGraph: result = graph.adj[node].len + int(node in graph.adj[node])
  of gkDiGraph: result = graph.adj[node].len
  of gkMultiGraph, gkMultiDiGraph:
    for neighbor in graph.multiadj[node].values:
      result.inc neighbor.len

proc size*(graph): int =
  case graph.kind
  of gkGraph, gkDiGraph, gkMultiDiGraph:
    for node, _ in graph.nodes: result += graph.degree node
    if graph.kind == gkGraph:
      result = result div 2
  of gkMultiGraph:
    var seen = HashSet[int]()
    for node, neighbors in graph.multiadj:
      for neighbor, unique in neighbors:
        for uid, _ in unique:
          if uid notin seen:
            seen.incl uid
    result = seen.len

proc `$`*(graph): string =
  fmt"{graph.kind}[{$graph.T}] on {graph.order} nodes with {graph.size} edges"

proc to_directed*(graph): Graph =
    result = Graph(nodes: graph.nodes)
    case graph.kind:
    of gkGraph, gkDiGraph:
      result.kind = gkDiGraph
      result.adj = graph.adj
    of gkMultiGraph, gkMultiDiGraph:
      result.kind = gkMultiDiGraph
      result.multiadj = graph.multiadj

proc to_undirected*(graph): Graph =
  result = Graph(nodes: graph.nodes)
  case graph.kind:
  of gkGraph, gkDiGraph:
    result.kind = gkGraph
    result.adj = graph.adj

    if graph.kind == gkDiGraph:
      for node, neighbors in result.adj:
        for neighbor, weight in neighbors:
          result.adj[neighbor] = weight

  of gkMultiGraph, gkMultiDiGraph:
    result.kind = gkMultiGraph
    result.multiadj = graph.multiadj

    if graph.kind == gkMultiDiGraph:
      for node, neighbors in result.multiadj:
        for neighbor, (_, weight) in neighbors:
          result.multiadj[neighbor][graph.uid] = weight
          inc graph.uid

when isMainModule:
  import std/sequtils
  # let edge_list = [(0, 0), (0, 1), (1, 0), (0, 2), (0, 3), (2, 3), (2, 3)]
  let edge_list = @[('a', 'a'), ('a', 'b'), ('b', 'a'), ('a', 'c'), ('a', 'd'), ('c', 'd'), ('c', 'd')]
  echo "edge list: ", edge_list
  for kind in GraphKind:
    var graph = kind.from_edges(edge_list)
    echo graph
    echo graph.edges.toSeq
