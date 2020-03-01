"""
CU
adjacency list indexed by vertex where for start_node n
adj[n] stores n's neighbours
"""


def bfs (start_node, adj):
    """
    :param n: the node we should start exploration from
    :type n: String
    :param adj: an adjacency list representation of the graph
    :type adj: dictionary of dictionaries
    """
    level = {start_node:None} #init at None
    parent = {start_node: None}
    frontier = [start_node] #limit between the explored and non-explored parts of the graph
    i = 1

    while frontier: #while not empty
        next = []
        for u in frontier: #nodes in the current "rim" being explored
            for v in adj[u]: #each child of these nodes
                if v not in level:
                    level[v] = i #set level of discovery
                    parent[v] = u #set parent in the current path
                    next.append(v) #becomes part of what should be explored next
                print("Visiting node: ", v )
        #prepare next iteration
        frontier = next
        i +=1

###############################################################################
test_graph_00 = {}
test_graph_00['A'] = {'B':True, 'E':{}}
test_graph_00['B'] = {'C':True, 'D':True}
test_graph_00['C'] = {}
test_graph_00['D'] = {}
test_graph_00['E'] = {}


bfs('A', test_graph_00)