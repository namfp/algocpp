__author__ = 'hnng'

import itertools


class Tree(object):
    def __init__(self, val, distance=0, left=None, right=None):
        self.distance = distance
        self.left = left
        self.right = right
        self.val = val

    def show(self, ord=''):
        print ord, self.val
        if self.left:
            self.left.show(ord=ord + '\t')
        if self.right:
            self.right.show(ord=ord + '\t')


    def insert_node(self, left_val, right_val):
        self.left = Tree(left_val, distance=self.distance + 1)
        self.right = Tree(right_val, distance=self.distance + 1)

    def insert_tree(self, left_tree, right_tree):
        self.left = left_tree
        if self.left:
            self.left.distance = self.distance + 1
        self.right = right_tree
        if self.right:
            self.right.distance = self.distance + 1

    def find_node(self, node_val):
        for node in self.traverse_tree():
            if node.val == node_val:
                return node
        return

    def traverse_tree(self):
        yield self
        if self.left:
            for tree in self.left.traverse_tree():
                yield tree
        if self.right:
            for tree in self.right.traverse_tree():
                yield tree

    def is_leaf(self):
        return not self.left and not self.right

    def iter_leaf(self):
        for tree in self.traverse_tree():
            if tree.is_leaf():
                yield tree

    def is_balanced(self):
        print "balanced"
        for first, second in itertools.combinations(self.iter_leaf(), 2):
            print first.val, first.distance, second.val, second.distance
            if abs(first.distance - second.distance) > 1:
                return False
        return True

    def find_node_trace(self, node_val):
        if node_val == self.val:
            return self, []
        else:
            found_left, trace_left = self.find_node_trace(self.left.val, [self])
            return




def test_tree():
    t = Tree(0)
    t.insert_node(1, 2)
    t.right.insert_node(3, 4)
    t.right.right.insert_node(5, 6)
    t.left.insert_node(7, 8)
    t.show()
    for node in t.iter_leaf():
        print node.val, node.distance
    print t.is_balanced()


def is_linked(graph, a, b):
    if a not in graph:
        return False
    for node in graph[a]:
        if node == b:
            return True
        if is_linked(graph, node, b):
            return True
    return False

def test_graph():
    graph = {
    1: [3, 4, 5],
    3: [7, 8],
    4: [3, 5, 2]
    }
    print is_linked(graph, 7, 8)
    print is_linked(graph, 1, 5)


def create_tree(sorted_list):
    if not sorted_list:
        return None
    if len(sorted_list) == 1:
        return Tree(val=sorted_list[0])

    index = len(sorted_list) / 2
    root = sorted_list[index]

    left = create_tree(sorted_list[:index]) if index > 0 else None
    right = create_tree(sorted_list[index + 1:]) if index < len(sorted_list) - 1 else None
    tree = Tree(root)
    tree.insert_tree(left, right)
    return tree


def find_commun_ancestor(tree, first, second):
    first_node = tree.find_node(first)
    second_node = tree.find_node(second)
    assert first_node
    assert second_node
    if first_node.parent == second_node:
        return second_node
    elif second_node.parent == first_node:
        return first_node
    else:
        return find_commun_ancestor(tree, first_node.parent, second_node.parent)





if __name__ == '__main__':
    tree = create_tree([1, 4, 5, 7, 9, 12, 15, 32, 41])
    print find_commun_ancestor(tree, 15, 41)
    tree.show()
