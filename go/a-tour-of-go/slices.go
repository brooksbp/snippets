package main

import "fmt"

func main() {
	p := []int{2, 3, 5, 7, 11, 13}

	fmt.Println("p ==", p)

	for i := 0; i < len(p); i++ {
		fmt.Printf("p[%d] == %d\n", i, p[i])
	}

	fmt.Println("p[:3] ==", p[:3])

	fmt.Println("p[4:] ==", p[4:])

	// slices are created with the make function. alloc zero'd array and
	// return a slice that refers to that array.
	a := make([]int, 5)
	printSlice("a", a)

	b := make([]int, 0, 5)
	printSlice("b", b)

	c := b[:2]
	printSlice("c", c)

	d := c[2:5]
	printSlice("d", d)

	// the zero value of a slice is nil.
	var z []int
	fmt.Println(z, len(z), cap(z))
	if z == nil {
		fmt.Println("nil!")
	}
}

func printSlice(s string, x []int) {
	fmt.Printf("%s len=%d cap=%d %v\n",
		s, len(x), cap(x), x)
}

// go's slice type provides a convenient and efficient means of working with
// sequences of typed data. Slices are analogous to arrays in other languages,
// but have some unusual properties.

// the
