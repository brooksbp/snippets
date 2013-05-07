#!/usr/bin/python3

import uuid
from datetime import datetime

def joinWords(words):
    '''slow string concatenation'''
    all = ""
    for w in words:
        all += w
    return all

def joinWords2(words):
    '''string .join method'''
    return ''.join(words)


l = []
for i in range(10000):
    l.append(str(uuid.uuid4()))


# 0:00:00.001478
# 0:00:00.000329

t1 = datetime.now()
tmp = joinWords(l)
print(datetime.now() - t1)

t1 = datetime.now()
tmp = joinWords2(l)
print(datetime.now() - t1)
