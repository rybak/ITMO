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
    for iter in range(maxIters):
        if debugOutput:
            print("Step {0}:".format(iter))
        # P step
        for user in range(nUsers):
            p[user] = ridgeRegression(q, userRates[user], L, 0)
        # Q step
        for item in range(nItems):
            q[item] = ridgeRegression(p, itemRates[item], L, 1)

    return lambda t: np.inner(p[t[0]], q[t[1]])


def ridgeRegression(q, userRates, L, saveCol):
    if len(userRates) == 0:
        return np.zeros(factors)

    A = np.zeros((factors, factors))
    d = np.zeros(factors)

    for item, rating in userRates:
        qq = q[item]
        A += np.outer(qq, qq)
        d += rating * qq

    for i in range(factors):
        A[i][i] += L * len(userRates)

    try:
        result = np.linalg.solve(A, d)
        result[saveCol] = 1
        return result
    except np.linalg.LinAlgError:
        return np.zeros(factors)


def getRates(nUsers, nItems, X, Y):
    userRates, itemRates = [[] for dummy in range(nUsers)], [[] for dummy in range(nItems)]

    for (user, item), rating in zip(X, Y):
        userRates[user].append((item, rating))
        itemRates[item].append((user, rating))

    return userRates, itemRates