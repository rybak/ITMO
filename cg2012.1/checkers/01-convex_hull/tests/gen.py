#!/usr/bin/python
from random import randint, uniform
from os.path import exists


nBounds     = [5, 10, 30, 200, 10000]
testsCount = [5, 10, 25, 20, 5]
xBounds     = [10, 100, 10000, 10000, 10000]
# testCount[i] tests with n <= nBounds[i] and maximum x\y <= xBounds[i]

sizes = []
bounds = []

for (nbound, testc, xbound) in zip(nBounds, testsCount, xBounds):
    for j in xrange(testc):
        sizes.append(randint(1, nbound))
        bounds.append(xbound)

def randPoint(maxX):
    return uniform(-maxX, maxX), uniform(-maxX, maxX)

print sizes
print bounds

for i in xrange(len(sizes)):
    fname = str(i + 1)
    fname = "0" * (3 - len(fname)) + fname
    f = open(fname + ".test", "w")
    points = []
    for j in xrange(sizes[i]):
        points.append(randPoint(bounds[i]))
    f.write(str(len(points)) + "\n")
    for x, y in points:
        f.write("{:.5f} {:.5f}\n".format(x, y))
    f.close()
