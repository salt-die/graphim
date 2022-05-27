import std/sets
import concepts

type
  NodeSet*[T: Hashable] = HashSet[T]

proc order*(nodes: NodeSet): int =
  nodes.len

proc addNode*[T](nodes: var NodeSet[T], node: T) =
  nodes.incl node

proc removeNode*[T](nodes: var NodeSet[T], node: T) =
  nodes.excl node

iterator nodes*[T](nodes: NodeSet[T]): T =
  for node in nodes.items:
    yield node

proc clear*(nodes: NodeSet) =
  nodes.clear