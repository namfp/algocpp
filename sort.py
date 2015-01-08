__author__ = 'hnng'


def merge_sort(l):
    if len(l) <= 1:
        return l
    else:
        mid = len(l) / 2
        return merge(merge_sort(l[:mid]), merge_sort(l[mid:]))


def merge(ll, lr):
    """
    :param ll: sorted list
    :param lr: sorted list
    :return: sorted list
    """
    result = []
    i = 0
    j = 0
    while True:
        if i >= len(ll):
            result.extend(lr[j:])
            return result
        elif j >= len(lr):
            result.extend(ll[i:])
            return result

        elif ll[i] < lr[j]:
            result.append(ll[i])
            i += 1
        else:
            result.append(lr[j])
            j += 1

def quicksort(l, begin, end):
    print l, begin, end
    if begin < end:
        return

    i = partition(l, begin, end)
    quicksort(l, begin, i)
    quicksort(l, i, end)


def partition(l, begin, end):
    n = l[end]
    print "partition", n, begin, end
    i = begin
    j = end
    while True:
        if l[i] < n:
            i += 1
            continue
        elif l[j] > n:
            j -= 1
            continue
        elif i >= j:
            return
        else:
            temp = l[i]
            l[i] = l[j]
            l[j] = temp
            j -= 1
    return



if __name__ == '__main__':
    # print merge([1], [4])
    # print merge_sort([1, 6, 3, 7, 5, 66, 20, 33, 45, 12, 15])
    print quicksort([7, 4, 8, 3, 5, 2, 22, 10], 0, 7)