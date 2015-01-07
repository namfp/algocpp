import sys, math

# Auto-generated code below aims at helping you parse
# the standard input according to the problem statement.


class Tree(object):
    def __repr__(self):
        return self.val

    def __init__(self, val=None):
        self.val = val
        self.childs = []

    def add_number(self, number):
        if not number:
            return
        for child in self.childs:
            if number[0] == child.val:
                del number[0]
                child.add_number(number)
                return
        new_child = Tree(number[0])
        self.childs.append(new_child)
        del number[0]
        new_child.add_number(number)

    def calculate(self):
        plus = 1 if self.val else 0
        return plus + sum([child.calculate() for child in self.childs])

    def show(self, order=''):
        print order + str(self.val)
        order += ' '
        for child in self.childs:
            child.show(order)





# N = int(raw_input())
# for i in xrange(N):
# telephone = raw_input()

# # Write an action using print
# # To debug: print >> sys.stderr, "Debug messages..."

# print "number" # The number of elements (referencing a number) stored in the structure.


if __name__ == '__main__':
    t = Tree()
    t.add_number(list('0123456789'))
    t.add_number(list('0123'))
    print t.calculate()
    t.show()
