package main

import (
	"fmt"
	"time"
)

func say(s string) {
	for i := 0; i < 5; i++ {
		time.Sleep(100 * time.Millisecond)
		fmt.Println(s)
	}
}

// a 'goroutine' is a lightweight thread managed by the Go runtime.

// 'go f(x,y,z)' starts a new goroutine running 'f(x,y,z)'

// the evaluation of f, x, y, z happens in current goroutine and
// the execution of f happens in the new goroutine

// same addr space, so access to shmem needs 'sync' primitives

func main() {
	go say("world")
	say("hello")
}
