#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from math import ceil
from random import shuffle

def sign(x):
    if x >= 0:
        return 1
    else:
        return -1

def unzip(*args):
    return list(zip(*args))

def split(ls, parts):
    n = len(ls)
    part_size = int(ceil(n / parts))
    for i in range(parts):
        yield ls[i * part_size:(i + 1) * part_size]

def split_data(xs, ys, parts):
    return list(split(unzip(xs, ys), parts))

def split_with_ratio(ls, ratio=0.5):
    size = round(len(ls) * ratio)
    return ls[:size], ls[size:]

def append(args):
    result = []
    for ls in args:
        result.extend(ls)
    return result

def average(ls):
    if len(ls) == 0:
        return 0
    return sum(ls) / len(ls)

def apply_permutation(ls, p):
    return [ls[x] for x in p]

def shuffle_args(*args):
    n = len(args[0])
    permutation = list(range(n))
    return [apply_permutation(x, permutation) for x in args]

def safe_division(x, y):
    if y == 0:
        return 0
    return x / y