__author__ = 'hnng'


def fibonancci(n):
    if n in [0, 1]:
        return n
    else:
        return fibonancci(n - 1) + fibonancci(n - 2)


def combinations_recursive(iterable, r):
    pass



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

        for j in range(i+1, r):
            indices[j] = indices[j-1] + 1
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


if __name__ == '__main__':
    # for c in combinations('abcdefgh', 3):
    #     print c

    print list_powerset([1, 2, 3])