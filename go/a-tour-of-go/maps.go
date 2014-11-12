package main

import "fmt"

type Vertex struct {
	Lat, Long float64
}

var m map[string]Vertex

func main() {
	m = make(map[string]Vertex)

	m["Bell Labs"] = Vertex{
		40.68433, -74.39967,
	}

	// insert or update an element:    m[key] = elem

	// retrieve an element:            elem = m[key]

	// delete:                         delete(m, key)

	// exists?                         elem, ok = m[key]
	// ok == true|false

	fmt.Println(m["Bell Labs"])
}

// map literals:
var z = map[string]Vertex{
	"Bell Labs": Vertex{
		40.68433, -74.39967,
	},
	"Google": Vertex{
		37.42202, -122.08408,
	},
}
var y = map[string]Vertex{
	"Bell Labs": {40.68433, -74.39967},
	"Google":    {37.42202, -122.08408},
}
