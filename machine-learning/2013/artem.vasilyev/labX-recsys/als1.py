import numpy as np

factors = 30
maxIters = 10
debugOutput = True


def learn(maxRating, nUsers, nItems, X, Y, L):
    userRates, itemRates = getRates(nUsers, nItems, X, Y)
    p = np.random.random((nUsers, factors))
    q = np.random.random((nItems, factors))
    for u in range(nUsers):
        p[u][0] = 1
    for i in range(nItems):
        q[i][1] = 1

    if debugOutput:
        print("Learning for L = {0}".format(L))
    for it in range(maxIters):
        if debugOutput:
            print("Step {0}:".format(it))
        # P step
        for user in range(nUsers):
            p[user] = ridgeRegression1(q, p[user], userRates[user], L, 0)
        # Q step
        for item in range(nItems):
            q[item] = ridgeRegression1(p, q[item], itemRates[item], L, 1)

    return lambda t: np.inner(p[t[0]], q[t[1]])


def ridgeRegression1(q, init, userRates, L, saveCol):
    if len(userRates) == 0:
        return init

    X = np.array([q[item] for item, rating in userRates]).T
    error = np.array([rating - np.inner(init, q[item]) for item, rating in userRates])
    precalcA = np.array([np.sum(x ** 2) for x in X])

    for k in range(factors):
        if k == saveCol:
            continue
        error += X[k] * init[k]
        init[k] = 0
        a, d = precalcA[k], sum(X[k] * error)
        init[k] = d / (L * len(userRates) + a)
        error -= X[k] * init[k]

    return init


def getRates(nUsers, nItems, X, Y):
    userRates, itemRates = [[] for dummy in range(nUsers)], [[] for dummy in range(nItems)]

    for (user, item), rating in zip(X, Y):
        userRates[user].append((item, rating))
        itemRates[item].append((user, rating))

    return userRates, itemRates