package main

import "fmt"

var i, j int = 1, 2
var c, python, java bool = true, false, false

func main() {
	fmt.Println(i, c, python, java)

	// inside a function := short assignment used in place of a var
	// declaration with implicit type
	k := 3
}
