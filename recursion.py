__author__ = 'hnng'


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


def eight_queens():
    result = []
    i = j = 0
    for i in range(8):
        pass







if __name__ == '__main__':
    # for c in combinations('abcdefgh', 3):
    # print c

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
