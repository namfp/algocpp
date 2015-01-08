__author__ = 'hibou'

import sys, math

# Auto-generated code below aims at helping you parse
# the standard input according to the problem statement.
import operator

n = 5
vs = '3 2 4 2 1 5'
ts = [int(v) for v in vs.split()]


def DivideAndConquerSingleSellProfit(arr, ord=''):
    # Base case: If the array has zero or one elements in it, the maximum
    # profit is 0.
    if len(arr) <= 1:
        return 0;

    # Cut the array into two roughly equal pieces.
    left = arr[: len(arr) / 2]
    right = arr[len(arr) / 2:]

    # Find the values for buying and selling purely in the left or purely in
    # the right.
    leftBest = DivideAndConquerSingleSellProfit(left, ord)
    rightBest = DivideAndConquerSingleSellProfit(right, ord)

    # Compute the best profit for buying in the left and selling in the right.
    crossBest = min(right) - max(left)

    # Return the best of the three
    return min(leftBest, rightBest, crossBest)


print DivideAndConquerSingleSellProfit(ts)





# Write an action using print
# To debug: print >> sys.stderr, "Debug messages..."

print "answer"


