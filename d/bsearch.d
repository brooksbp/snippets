#!/usr/bin/rdmd

import std.array;

unittest {
    assert(binarySearch([1,2,3,7,8,9], 3));
    assert(!binarySearch([1,2,3,7,8,9], 4));
}

bool binarySearch(T)(T[] input, T value)
{
    while (!input.empty)
    {
        auto i = input.length / 2;
        auto mid = input[i];

        if (mid > value)
            input = input[0 .. i];
        else if (mid < value)
            input = input[i + 1 .. $];
        else
            return true;
    }
    return false;
}

bool rBinarySearch(T)(T[] input, T value)
{
    if (input.empty)
        return false;

    auto i = input.length / 2;
    auto mid = input[i];

    if (mid > value)
        return rBinarySearch(input[0 .. i]);
    if (mid < value)
        return rBinarySearch(input[i + 1 .. $]);

    return true;
}

void main()
{
    
}