# Given BST, how many permu's produce same BST?

tree = {
    'C' : ['B', 'P'],
    'P' : ['H', 'Z'],
    'H' : ['D']
}

def f(tree, ready):
    if not ready:
        return [[]]
    else:
        rv = []
        for r in ready:
            for rest in f(tree,
                          [n for n in ready if r != n] + tree.get(r, [])):
                rv.append([r] + rest)
        return rv

for o in f(tree, 'C'):
    print ''.join(o)

