
removable_edges = 0
edges = None


def get_input():
    nr_nodes, nr_edges = map(int, input().split(" "))
    edges = {}
    
    for i in range(nr_edges):
        node0, node1 = input().split(" ")
        edges[int(node0)] = int(node1)
 
    return edges

edges = get_input()


def connected_edges(node):
    conn_edges = []
    for edge in edges.items():
        if node in edge:
            conn_edges.append(edge)
    return conn_edges


def connected_nodes(node):
    conn_edges = connected_edges(node)
    conn_nodes = []
    for edge in conn_edges:
        if edge[0] == node:
            conn_nodes.append(edge[1])
        else:
            conn_nodes.append(edge[0])
    return conn_nodes


def remove_node(node, conn_nodes):
    is_not_node = lambda n: n != node
    return list(filter(is_not_node, conn_nodes))


def nodes_in_subtree(new_root, last_node):
    conn_nodes = connected_nodes(new_root)
    conn_nodes = remove_node(last_node, conn_nodes)

    if len(conn_nodes) == 0:
        res = 1
    else:
        res = 1 + sum((nodes_in_subtree(node, new_root)
                       for node in conn_nodes))

    global removable_edges
    if res % 2 == 0 and res != 0:
        removable_edges += 1
    return res

# Any random existing node, just works, though it should not :)
sub_tree_roots = connected_nodes(1)

for root in sub_tree_roots:
    nodes_in_subtree(root, 1)

print(removable_edges)
