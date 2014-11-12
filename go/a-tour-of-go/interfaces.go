package main

import (
	"fmt"
	"math"
)

// An interface type is defined by a set of methods.

type Abser interface {
	Abs() float64
}

// A value of interface type can hold any value that implements
// those methods.

func main() {
	var a Abser

	f := MyFloat(-math.Sqrt2)
	v := Vertex{3, 4}

	a = f  // a MyFloat implements Abser
	a = &v // a *Vertex implements Abser

	// a = v .. Vertex does not implement Abser (*Vertex does though)

	fmt.Println(a.Abs())
}

type MyFloat float64

func (f MyFloat) Abs() float64 {
	if f < 0 {
		return float64(-f)
	}
	return float64(f)
}

type Vertex struct {
	X, Y float64
}

func (v *Vertex) Abs() float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y)
}
