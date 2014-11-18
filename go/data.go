package main

import (
	"fmt"
)

func main() {
	// Array types
	// ===========
	//
	// An array is a numbered sequence of elements of a single type, called
	// the element type.
	//
	// ArrayType   = "[" ArrayLength "]" ElementType .
	// ArrayLength = Expression .
	// ElementType = Type .
	//
	// The length is part of the array's type; it must evaluate to a non-
	// negative constant representable by a value of type int.
	//   len(a) - length of array a
	//   element access by integer indices 0 .. len(a)-1
	// Ex:
	//   [32]byte
	//   [2*N] struct { x, y int32 }
	//   [1000]*float64
	//   [3][5]int
	//   [2][2][2]float64  // same as [2]([2]([2]float64))

	// Slice types
	// ===========
	//
	// A slice is a descriptor for a contiguous segment of an underlying
	// array and provides access to a numbered sequence of elements. A slice
	// type denotes the set of all slices of arrays of its element type. The
	// value of an uninitialized slice is 'nil'.
	//
	// SliceType = "[" "]" ElementType .
	//
	// Like arrays, slices are indexable and have a length. Unlike arrays,
	// the length may change during execution. The slice index of a given
	// element may be less than the index of the same element in the array.
	//
	// A slice, onced initialized, is always associated with an underlying
	// array that holds its elements. A slice therefore shares storage with
	// its array and with other slices of the same array; by contrast,
	// distinct arrays always represent distinct storage.
	//
	// The array underlying a slice may extend past the end of the slice.
	// The capacity is a measure of that extent: it is the sume of the
	// length of the slice and the length of the array beyond the slice; a
	// slice of length up to that capacity can be created by slicing a new
	// one from the original slice.
	//   len(s) - length
	//   cap(s) - capacity
	//
	// A new, initialized slice value for a given element type T is made
	// using the built-in function 'make', which takes a slice type and
	// parameters specifying the length and optionally the capacity. A
	// slice created with 'make' always allocates a new, hidden array to
	// which the returned slice value refers. That is, executing
	//
	//   make([]T, length, capacity)
	//
	// produces the same slice as allocating an array and slicing it, so the
	// following two expressions are equivalent to each other
	//
	//   make([]int, 50, 100)
	//   new([100]int)[0:50]

	// Type identity
	// =============
	//
	// Types are either 'identical' or 'different'.
	//
	// Two unnamed types are identical if:
	//
	// - two array types are identical if they have identical element types
	//   and the same array length.
	// - two slice types are identical if they have identical element types.
	// - two struct types are identical if they have the same sequence of
	//   fields, and if corresponding fields have the same names, and
	//   identical types, and identical tags. Two anon fields are considered
	//   to have the same name. Lower-case field names from different
	//   packages are always different.
	// - two pointer types are identical if they have identical base types.
	// ..

	// Slice expressions
	// =================
	//
	// Slice expressions construct a substring or slice from a string,
	// array, pointer to array, or slice. Simple form, and full form which
	// also specifies a bound on the capacity.
	//
	// For a string, array, pointer to array, or slice 'a', the primary
	// expression
	//
	//   a[low : high]
	//
	// constructs a substring or slice.
	//

	a := [5]int{1, 2, 3, 4, 5} // [1 2 3 4 5]
	s := a[1:4]                // [2 3 4]

	// a[2:]      a[2 : len(a)]
	// a[:3]      a[0 : 3]
	// a[:]       a[0 : len(a)]

	// TODO: selectors have strange dereferencing sugar for ptr types..

	// a[low : high : max]
	//
	// constructs a slice like simple form, but sets resulting slice's
	// capacity to max-low

	t := a[1:3:5] // [2 3] cap=4

	// Allocation
	// ==========
	//
	// The built-in function 'new' takes a type T and returns a value of
	// type *T. For instance
	//
	//   type S struct { a int; b float64 }
	//   new(S)
	//
	// dynamically allocates memory for a variable of type S, initializes it
	// (a=0, b=0.0), and returns a value of type *S containing the address
	// of the memory.

	// Making slices, maps, and channels
	// =================================
	//
	// The built-in function 'make' takes a type T, which must be a slice,
	// map, or channel type, optionally followed by a type-specific list of
	// expressions. It returns a value of type T (not *T).
	//
	// Call           Type T   Result
	//
	// make(T, n)     slice    slice of type T with len n and cap n
	// make(T, n, m)  slice    slice of type T with len n and cap m
	//
	// make(T)        map      map of type T
	// make(T, n)     map      map of type T with initial space for n elems
	//
	// make(T)        channel  unbuffered channel of type T
	// make(T, n)     channel  buffered channel of type T, buffer size n

	// Appending to and copying slices
	// ===============================
	//
	// The built-in functions 'append' and 'copy' assist in common slice
	// operations. For both functions, the result is independent of whether
	// the memory referenced by the arguments overlaps.
	//
	// The variadic function 'append' appends zero or more values 'x' to 's'
	// of type S, which must be a slice type, and returns the resulting
	// slice, also of type S.
	//
	//   append(s S, x ...T) S    // T is the element type of S
	//
	// If the capacity of 's' is not large enough to fit the additional
	// values, 'append' allocates a new, sufficiently large underlying array
	// that fits both the existing slice elements and the additional values.
	// Otherwise, 'append' re-uses the underlying array.

	s0 := []int{0, 0}
	s1 := append(s0, 2)              // s1 == []int{0,0,2}
	s2 := append(s1, 3, 5, 7)        // s2 == []int{0,0,2,3,4,7}
	s3 := append(s2, s0...)          // s3 == []int{0,0,2,3,4,7,0,0}
	s4 := append(s3[3:6], s3[2:]...) // s4 == []int{3,5,7,2,3,5,7,0,0}

	// Schematically, 'append' is like this:
	//
	//   func append(slice []T, elements ...T) []T
	//
	// where T is a placeholder for any given type. You can't actually write
	// a function in Go where the type T is determined by the caller. That's
	// why 'append' is built in: it needs support from the compiler.
	//
	x := []int{1, 2, 3}
	y := []int{4, 5, 6}
	x = append(x, y...)

	// Naive:
	func Append(slice, data []byte) []byte {
		l := len(slice)
		if l + len(data) > cap(slice) { // realloc
			newSlice := make([]byte, (l+len(data))*2) // double..
			copy(newSlice, slice)
			slice = newSlice
		}
		slice = slice[0:l+len(data)]
		for i, c := range data {
			slice[l+i] = c
		}
		return slice
	}
	// The function 'copy' copies slice elements from a 'src' to 'dst' and
	// returns the num elems copied. Both arguments must have identical
	// element type T and must be assignable to a slice of type []T. The num
	// elems copied is min(len(src), len(dst)).
	//
	//   copy(dst, src []T) int
	//   copy(dst []byte, src string) int    // special case for str->[]byte

	var a = [...]int{0, 1, 2, 3, 4, 5, 6, 7}
	var s = make([]int, 6)
	var b = make([]byte, 5)

	n1 := copy(s, a[0:])           // n1 == 6, s == []int{0,1,2,3,4,5}
	n2 := copy(s, s[2:])           // n2 == 4, s == []int{2,3,4,5,4,5}
	n3 := copy(b, "Hello, World!") // n3 == 5, b == []byte("Hello")
}
