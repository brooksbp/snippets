package main

import (
	"fmt"
	"math"
)

// Go does not have classes. You can define methods on struct types.

// the 'method receiver' appears in its own argument list btwn the
// 'func' keyword and the method name.

type Vertex struct {
	X, Y float64
}

func (v *Vertex) Abs() float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y)
}

// you can define a method on any type in your pkg.. not just structs

// you cannot define a method on a type from another package or
// on a basic type

type MyFloat float64

func (f MyFloat) Abs() float64 {
	if f < 0 {
		return float64(-f)
	}
	return float64(f)
}

// use pass-by-pointer for:
// 1. avoid large copy of pass-by-value
// 2. if you need to mutate the object

func main() {
	v := &Vertex{3, 4}
	fmt.Println(v.Abs())

	f := MyFloat(-math.Sqrt2)
	fmt.Println(f.Abs())
}
