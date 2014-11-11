package main

import "fmt"

type Vertex struct {
	X, Y int
}

var (
	p = Vertex{1, 2}  // has type Vertex
	q = &Vertex{1, 2} // has type *Vertex
	r = Vertex{X: 1}  // Y:0 is implicit
	s = Vertex{}      // X:0 and Y:0
)

func main() {
	fmt.Println(p, q, r, s)

	v := new(Vertex) // new allocates a zeroed value
	fmt.Println(v)
	v.X, v.Y = 11, 9
	fmt.Println(v)
}
