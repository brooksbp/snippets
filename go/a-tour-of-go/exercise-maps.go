package main

import (
	"code.google.com/p/go-tour/wc"
	"strings"
)

func WordCount(s string) map[string]int {
	m := make(map[string]int)

	words := strings.Fields(s)
	for _, w := range words {
		m[w]++
	}

	return m
}

func main() {
	wc.Test(WordCount)
}
