# 19**92 mod 92

print pow(19, 92, 92)

# Memory-efficient method:
#
#   c = (a x b) mod m
#   c = (a x (b mod m)) mod m

def pow2(b, e, m):
    c = 1
    i = 0
    while i < e:
        i = i + 1
        c = (c * b) % m
    return c

print pow2(19, 92, 92)

# TODO: right-to-left binary method
