package main

import (
	"fmt"
	"math"
)

// returns a closure. each one is bound to its own 'sum' variable
func adder() func(int) int {
	sum := 0
	return func(x int) int {
		sum += x
		return sum
	}
}

func main() {
	hypot := func(x, y float64) float64 {
		return math.Sqrt(x*x + y*y)
	}
	fmt.Println(hypot(3, 4))

	// functions may be closures. A closure is a function value that
	// references variables from outside its body. The function may
	// access and assign to the referenced variables; in this sense
	// the function is "bound" to the variables.
	pos, neg := adder(), adder()
	for i := 0; i < 10; i++ {
		fmt.Println(
			pos(i),
			neg(-2*i),
		)
	}
}
