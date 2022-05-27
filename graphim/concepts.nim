import std/hashes

type
  Hashable* = concept h
    hash h is int

  NodeStorage*[T: Hashable] = concept n
    n.has(T) is bool
    for node in n.nodes: node is T
    n.order is int
    n.addNode T
    n.removeNode T
    n.clear

  EdgeStorage*[T: Hashable] = concept e
    (T, T) in e is bool
    for node in e.successors(T): node is T
    for edge in e.edges: edge is (T, T)
    e.isDirected is bool
    e.outDegree(T) is int
    e.size is int
    e.addNode T
    e.removeNode T
    e.addEdge (T, T)
    e.removeEdge (T, T)
    e.clearEdges

template makeGraphImpl*(T: Hashable, N, E: untyped): untyped {.dirty.} =
  assert N[T] is NodeStorage
  assert E[T] is EdgeStorage

  import std/[sets, strformat, tables]

  type
    Graph* = object
      nodestorage: N[T]
      edgestorage: E[T]

  proc newGraph*: Graph =
    Graph(nodestorage: N[T](), edgestorage: E[T]())

  using G: Graph

  iterator nodes*(G): T =
    for node in G.nodestorage.nodes: yield node

  iterator successors*(G; node: T): T =
    for succ in G.edgestorage.successors(node): yield succ

  iterator edges*(G): (T, T) =
    for edge in G.edgestorage.edges: yield edge

  proc isDirected*(G): bool =
    G.edgestorage.isDirected

  proc contains*(G; node: T): bool =
    G.nodestorage.has node

  proc contains*(G; edge: (T, T)): bool =
    edge in G.edgestorage

  proc outDegree*(G; node: T): int =
    G.edgestorage.outDegree node

  proc degreeHistogram*(G): CountTable[int] =
    for node in G.nodestorage.nodes: result.inc G.outDegree(node)

  proc order*(G): int = G.nodestorage.order

  proc size*(G): int = G.edgestorage.size

  proc `$`*(G): string =
    fmt"Graph on {G.order} nodes with {G.size} edges."

  proc addNode*(G: var Graph, node: T) =
    G.nodestorage.addNode node
    G.edgestorage.addNode node

  proc removeNode*(G: var Graph, node: T) =
    G.nodestorage.removeNode node
    G.edgestorage.removeNode node

  proc addEdge*(G: var Graph, edge: (T, T)) =
    let (u, v) = edge
    G.addNode u
    G.addNode v
    G.edgestorage.addEdge edge

  proc removeEdge*(G: var Graph, edge: (T, T)) =
    G.edgestorage.removeEdge edge

  proc addNodesFrom*(G: var Graph, nodes: openArray[T]) =
    for node in nodes: G.addNode node

  proc removeNodesFrom*(G: var Graph, nodes: openArray[T]) =
    for node in nodes: G.removeNode node

  proc addEdgesFrom*(G: var Graph, edges: openArray[(T, T)]) =
    for edge in edges: G.addEdge edge

  proc removeEdgesFrom*(G: var Graph, edges: openArray[(T, T)]) =
    for edge in edges: G.removeEdge edge

  proc clear*(G: var Graph) =
    G.nodestorage.clear
    G.edgestorage.clear

  proc clearEdges*(G: var Graph) =
    G.edgestorage.clear

  proc update*(G: var Graph, H: Graph) =
    for node in H.nodes:
      G.addNode node

    for edge in H.edges:
      G.addEdge edge

  proc inducedSubgraph*(G: var Graph, nodes: openArray[T]): Graph =
    result.nodestorage = N[T]()
    result.edgestorage = E[T]()

    let keep = toHashSet nodes

    for node in nodes:
      if node in G:
        for succ in G.successors(node):
          if succ in keep:
            result.addEdge (node, succ)
