#!/usr/bin/python

import random

def r():
    return str(random.randrange(0, 999999999))

for i in range(256/4):
    print '\t' + r() + ', ' + r() + ', ' + r() + ', ' + r() + ','
    

