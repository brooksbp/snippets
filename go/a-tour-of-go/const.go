package main

const Pi = 3.14

const (
	Big   = 1 << 100
	Small = Big >> 99
)

// numeric constants are high-precision values. an untyped constant takes the
// type needed by its context.

func main() {
	const World = "blah"
}
