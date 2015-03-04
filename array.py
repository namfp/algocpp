__author__ = 'hnng'


def rotate(matrix):
    n = len(matrix)
    m = len(matrix[0])
    result = [[0 for j in range(m)] for i in range(n)]
    for i in range(n):
        for j in range(m):
            result[j][m - i - 1] = matrix[i][j]
    return result

if __name__ == '__main__':
    matrix = [[1, 2, 3],
              [4, 5, 6],
              [7, 8, 9]]

    for row in matrix:
        print row
    print "=============="
    for row in  rotate(matrix):
        print row
