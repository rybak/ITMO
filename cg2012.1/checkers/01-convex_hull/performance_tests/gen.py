#!/usr/bin/python
from random import *

tests = 3

n = 600000
bound = 1000.0

for test in range(tests):
    f = open("test{}".format(test), "w")
    f.write("{}\n".format(n))
    for i in xrange(n):
        x = uniform(-bound, bound)
        y = uniform(-bound, bound)
        f.write("{:.2f} {:.2f}\n".format(x, y))
    f.close()
