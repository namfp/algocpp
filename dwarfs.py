__author__ = 'hibou'


class Node(object):
    def __init__(self, val, parent=None):
        self.parent = parent
        self.val = val
        self.childs = []

    def find_node(self, node_val):
        found = None
        if node_val == self.val:
            found = self
        else:
            for child in self.childs:
                found = child.find_node(node_val)
                if found:
                    break
        return found

    def set_parent(self, node):
        self.parent = node
        existed = [n for n in node.childs if n.val == self.val]
        assert len(existed) < 2
        if not existed:
            node.childs.append(self)

    def length(self):
        if not self.childs:
            return 1
        else:
            return 1 + max([child.length() for child in self.childs])




class Graph(object):
    def __init__(self):
        self.distinct_nodes = []

    def search_node(self, node_val):
        found_node = None
        for node in self.distinct_nodes:
            found_node = node.find_node(node_val)
            if found_node:
                break
        return found_node

    def add_link(self, node_parent_val, node_child_val):
        node1 = self.search_node(node_parent_val)
        if not node1:
            node1 = Node(node_parent_val)
            self.distinct_nodes.append(node1)

        node2 = self.search_node(node_child_val)
        if not node2:
            node2 = Node(node_child_val)
        node2.set_parent(node1)

    def compute_length(self):
        return max([node.length() for node in self.distinct_nodes])


if __name__ == '__main__':
    g = Graph()
    g.add_link(1, 2)
    g.add_link(1, 3)
    g.add_link(3, 4)
    g.add_link(2, 4)
    g.add_link(2, 5)
    g.add_link(10, 11)
    g.add_link(10, 1)
    g.add_link(10, 3)
    print g.compute_length()






