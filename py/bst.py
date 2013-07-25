
class Node():
    def __init__(self, data):
        self.data = data
        self.left = None
        self.right = None

def is_bst(root):
    (ordered, _) = is_bst_(root)
    return ordered

def is_bst_(node):
    left = False
    right = False
    max_data = node.data
    if not node.left and not node.right:
        return (True, max_data)
    if node.left:
        (l_ord, l_max) = is_bst_(node.left)
        if l_ord and l_max < node.data:
            left = True
        else:
            return (False, 0)
    if node.right:
        (r_ord, r_max) = is_bst_(node.right)
        if r_ord and r_max > node.data:
            right = True
            max_data = r_max
    return (not node.right or (node.right and right), max_data)

n1 = Node(1)
n2 = Node(2)
n3 = Node(3)
n4 = Node(4)
n5 = Node(5)
n6 = Node(6)

n5.left = n3
n5.right = n6
n3.left = n1
n3.right = n2
assert is_bst(n5) is False
n3.right = n4
assert is_bst(n5) is True
