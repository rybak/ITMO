import subprocess
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
    f = open("test" + str(i + 1), "w")
    points = []
    for j in xrange(sizes[i]):
        points.append(randPoint(bounds[i]))
    f.write(str(len(points)) + "\n")
    for x, y in points:
        f.write("%f %f\n" % (x, y))
    f.close()
