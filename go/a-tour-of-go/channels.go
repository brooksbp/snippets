package main

import "fmt"

func sum(a []int, c chan int) {
	sum := 0
	for _, v := range a {
		sum += v
	}
	c <- sum // send sum to c
}

// channels are a typed conduit through which you can send and receive
// values with the channel operator <-

// by default tx and rx block until the other side is ready. This allows
// goroutines to synchronize without explicit locks or cond vars.

func main() {
	a := []int{7, 2, 8, -9, 4, 0}

	c := make(chan int)

	go sum(a[:len(a)/2], c)
	go sum(a[len(a)/2:], c)

	x, y := <-c, <-c // receive from c

	fmt.Println(x, y, x+y)

	// channels can be buffered. provide the buffer length...
	myChan := make(chan int, 2)
	myChan <- 1
	myChan <- 2
	fmt.Println(<-myChan)
	fmt.Println(<-myChan)

	// tx to a buffered channel block only when buffer full
	// rx blocks when buffer is empty.
}
