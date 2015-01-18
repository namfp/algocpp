__author__ = 'hnng'
import math

def fibonancci(n):
    if n in [0, 1]:
        return n
    else:
        return fibonancci(n - 1) + fibonancci(n - 2)


def combinations_recursive(iterable):
    if not iterable:
        return [tuple()]
    first = (iterable[0],)
    subset = combinations_recursive(iterable[1:])
    return [first + elem for elem in subset] + [first]


def combinations(iterable, r):
    # combinations('ABCD', 2) --> AB AC AD BC BD CD
    # combinations(range(4), 3) --> 012 013 023 123
    pool = tuple(iterable)
    n = len(pool)
    if r > n:
        return
    indices = range(r)
    print indices
    print tuple(pool[i] for i in indices)
    yield tuple(pool[i] for i in indices)
    while True:
        for i in reversed(range(r)):
            if indices[i] != i + n - r:
                break
        else:
            return
        indices[i] += 1

        for j in range(i + 1, r):
            indices[j] = indices[j - 1] + 1
        print indices
        yield tuple(pool[i] for i in indices)


def list_powerset(lst):
    # the power set of the empty set has one element, the empty set
    result = [[]]
    for x in lst:
        # for every additional element in our set
        # the power set consists of the subsets that don't
        # contain this element (just take the previous power set)
        # plus the subsets that do contain the element (use list
        # comprehension to add [x] onto everything in the
        # previous power set)
        print "x", x
        print "result", result
        temp = [subset + [x] for subset in result]
        print "temp", temp
        result.extend(temp)
    return result


def all_perms(elements):
    if len(elements) <= 1:
        yield elements
    else:
        for perm in all_perms(elements[1:]):
            for i in range(len(elements)):
                # nb elements[0:1] works in both string and list contexts
                yield perm[:i] + elements[0:1] + perm[i:]


def colorized(pixels, i, j, new_color):
    l = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    computed = set()

    to_be_computed = [(i, j)]
    old_color = pixels[i][j]
    while to_be_computed:
        compute_i, compute_j = to_be_computed[-1]
        to_be_computed.pop()
        pixels[compute_i][compute_j] = new_color
        computed.add((compute_i, compute_j))
        for ii, jj in l:
            new_i = compute_i + ii
            new_j = compute_j + jj
            if 0 <= new_i < len(pixels) and 0 <= new_j < len(pixels[0]) \
                    and (new_i, new_j) not in computed and (new_i, new_j) not in to_be_computed \
                    and pixels[new_i][new_j] == old_color:
                to_be_computed.append((new_i, new_j))


def test_colorized():
    m = [['X', 'X', 'X', 'X', 'X', 'X'],
         ['X', 'X', 'Y', 'Y', 'X', 'X'],
         ['X', 'X', 'X', 'Y', 'Y', 'X'],
         ['X', 'X', 'X', 'X', 'X', 'X'],
         ['X', 'X', 'X', 'X', 'X', 'X'],
    ]
    for l in m:
        print l
    colorized(m, 1, 2, 'Z')
    print "============================="
    for l in m:
        print l


def reverse_list(l):
    if len(l) == 1:
        return l
    else:
        return [l[-1]] + reverse_list(l[:-1])


import turtle


def tree(branchLen, t):
    if branchLen > 5:
        t.forward(branchLen)
        t.right(20)
        tree(branchLen - 15, t)
        t.left(40)
        tree(branchLen - 15, t)
        t.right(20)
        t.backward(branchLen)


def test_tree():
    t = turtle.Turtle()
    myWin = turtle.Screen()
    t.left(90)
    t.up()
    t.backward(100)
    t.down()
    t.color("green")
    tree(75, t)
    myWin.exitonclick()


def moveTower(height, fromPole, toPole, withPole):
    if height >= 1:
        moveTower(height - 1, fromPole, withPole, toPole)
        moveDisk(fromPole, toPole)
        moveTower(height - 1, withPole, toPole, fromPole)


def move_tower_iterative(height, from_pole, to_pole, with_pole):
    stack_move_first = []
    stack_move_after = []
    height_first = height
    while height_first >= 1:
        stack_move_after.append((height_first - 1, from_pole, with_pole, to_pole))

    pass


def moveDisk(fp, tp):
    print("moving disk from", fp, "to", tp)


def hilber_curve():
    t = turtle.Turtle()
    myWin = turtle.Screen()
    t.left(90)
    t.up()
    t.backward(100)
    t.down()
    t.color("green")
    myWin.exitonclick()


def pascal(n, spaces=0):
    to_print = '   ' * spaces
    if n == 0:
        print to_print + '1'
    else:
        pascal(n - 1, spaces + 1)
        for i, k in enumerate(range(n + 1)):
            if k == 0:
                to_print += str(math.factorial(n) / (math.factorial(i) * math.factorial(n - i)))
            else:
                to_print += '   ' + str(math.factorial(n) / (math.factorial(i) * math.factorial(n - i)))
        print to_print




if __name__ == '__main__':
    pascal(15)