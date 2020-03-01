"""
CU
adjacency list indexed by vertex where for start_node n
adj[n] stores n's neighbours
"""

def dfs (node, adj):
    """
    :param node: the node we should start exploration from
    :type node: String
    :param adj: an adjacency list representation of the graph
    :type adj: dictionary of dictionaries
    """
    parent = {node:None}
    for v in adj[node]: #for every single node
        if v not in parent: #ensures we only visit every vertex once!
            parent[v] = node
            dfs(v, adj)
            print("Visiting node: ", v)


###############################################################################

test_graph_00 = {}
test_graph_00['A'] = {'B':True, 'E':{}}
test_graph_00['B'] = {'C':True, 'D':True}
test_graph_00['C'] = {}
test_graph_00['D'] = {}
test_graph_00['E'] = {}


dfs('A', test_graph_00)