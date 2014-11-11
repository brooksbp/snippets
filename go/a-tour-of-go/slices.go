package main

import "fmt"

func main() {
	p := []int{2, 3, 5, 7, 11, 13}

	fmt.Println("p ==", p)

	for i := 0; i < len(p); i++ {
		fmt.Printf("p[%d] == %d\n", i, p[i])
	}
}

// go's slice type provides a convenient and efficient means of working with
// sequences of typed data. Slices are analogous to arrays in other languages,
// but have some unusual properties.

// the
