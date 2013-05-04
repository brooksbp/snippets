def myDrop(n, xs):
    while n > 0 and xs:
        n = n - 1
        xs = xs[1:]
    return xs
