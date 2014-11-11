package main

import "fmt"

// go has only one looping construct, the 'for' loop.

func main() {
	sum := 0
	for i := 0; i < 10; i++ {
		sum += i
	}
	fmt.Println(sum)

	// use 'for' as 'while':
	sum = 1
	for sum < 1000 {
		sum += sum
	}
	fmt.Println(sum)

	// for {
	//   .. forever ..
	// }
}
