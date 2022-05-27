import ../graphim/[concepts, node_set, edge_table_set]

makeGraphImpl(int, NodeSet, EdgeTableSet)

var G = newGraph()

G.addEdge (1, 2)
G.addEdge (1, 3)
echo G
G.removeEdge (3, 1)
echo G
clear G
echo G