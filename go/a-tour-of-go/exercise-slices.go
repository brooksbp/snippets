package main

import "code.google.com/p/go-tour/pic"

func Pic(dx, dy int) [][]uint8 {
	p := make([][]uint8, dy)
	for y := range p {
		p[y] = make([]uint8, dx)
		for x := range p[y] {
			p[y][x] = uint8(x ^ y)
		}
	}
	return p
}

func main() {
	pic.Show(Pic)
}
