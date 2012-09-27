#!/usr/bin/env python

import logging


# loop invariant -
#   1) it is true prior to first iteration.
#   2) if it is true before an iteration, it remains true before next iteration.
#   3) upon termination, invariant provides useful property to help show correctness.


def insertion_sort(A):
    # loop invariant: A[0..j-1] is sorted.
    for j in range(1, len(A)):
        key = A[j]
        i = j - 1
        logging.debug('i=%d j=%d key %d  ' % (i, j, A[j]) + str(A))
        while i >= 0 and A[i] > key:
            A[i + 1] = A[i]
            i = i - 1
        A[i + 1] = key
    logging.info('returning: ' + str(A))
    return A


def merge(A, p, q, r):
    logging.debug('A=%s p=%d q=%d r=%d' % (str(A), p, q, r))
    n1 = q - p + 1
    n2 = r - q
    print n1
    print n2
    # L[1..n1+1], R[1..n2+1]
    L, R = [], []
    for i in range(0, n1):
        L[i] = A[p + i - 1]
    for j in range(0, n2):
        R[j] = A[q + j]
    print L
    print R
    L[n1 + 1] = None
    R[n2 + 1] = None
    i, j = 1, 1
    # loop invariant: A[p..k-1] contains the k - p smallest elements of L[1..n1+1] and
    #                 R[1..n2+1] in sorted order. L[i] and R[j] are the smallest
    #                 elements of their arrays that have not been copied back to A.
    for k in range(p, r):
        if L[i] <= R[j]:
            A[k] = L[i]
            i = i + 1
        else:
            A[k] = R[j]
            j = j + 1

def merge_sort(A, p, r):
    if p < r:
        q = (p + r) / 2
        merge_sort(A, p, q)
        merge_sort(A, q + 1, r)
        merge(A, p, q, r)


def main():
    unsorted = [5, 2, 4, 6, 1, 3]
    assert insertion_sort([5, 2, 4, 6, 1, 3]) == [1, 2, 3, 4, 5, 6]
    assert merge_sort([5, 2, 4, 6, 1, 3], 0, len(unsorted)) == [1, 2, 3, 4, 5, 6]

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    main()
