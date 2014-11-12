package main

import "fmt"

// fibonacci is a function that returns
// a function that returns an int.
func fibonacci() func() int {
	n := 0
	a := 0
	b := 1
	return func() int {
		var c int
		if n == 0 {
			c = a
			n++
		} else if n == 1 {
			c = b
			n++
		} else {
			c = a + b
			a = b
			b = c
		}
		return c
	}
}

func main() {
	f := fibonacci()
	for i := 0; i < 10; i++ {
		fmt.Println(f())
	}
}
