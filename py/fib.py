
def fib(n):
    if n == 1 or n == 2:
        return 1
    else:
        return fib(n - 2) + fib(n - 1)

def fib2(n):
    a = 1
    b = 1
    c = 0

    if n == 1 or n == 2:
        c = 1
    else:
        i = n - 2
        while i:
            i -= 1
            c = a + b
            a = b
            b = c
    return c


#print(fib(46))
print(fib2(46))
