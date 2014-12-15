package abc123

// -----------------------------------------------------------------------------
// Channel types

// A channel provides a mechanism for concurrently executing functions to
// communicate by sending and receiving values of a specified element type.

// The value of an uninitialized channel is nil.

// ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .

// The optional <- operator specifies the channel _direction_, send or receive.
// If no direction is given, the channel is _bidirectional_. A channel may be
// constrained only to send or only to receive by conversion or assignment.

chan T          // can be used to send and receive values of type T
chan<- float64  // can only be used ot send float64s
<-chan int      // can only be used to receive ints

// The <- operator associates with the leftmost chan possible:

chan<- chan int    // chan<- (chan int)
chan<- <-chan int  // chan<- (<-chan int)
<-chan <-chan int  // <-chan (<-chan int)
chan (<-chan int)

// A new, initialized channel value can be made using the built-in function make,
// which takes the channel type and an optional capacity as arguments

make(chan int, 100)

// The capacity, in number of elements, sets the size of the buffer in the
// channel. If the capacity is zero or absent, the channel is unbuffered and
// communication succeeds only when both a sender and receiver are ready.
// Otherwise, the channel is buffered and communication succeeds without
// blocking if the buffer is not full (sends) or not empty (receives). A nil
// channel is never ready for communication.

// A channel may be closed with the built-in function close. The multi-valued
// assignment form of the receive operator reports whether a received value was
// sent before the channel was closed.

// A single channel may be used in 'send statements', 'receive operations', and
// calls to the built-in functions 'cap' and 'len' by any number of goroutines
// without further synchronization.

// Channels act as FIFO queues. For example, if one goroutine sends values on a
// channel and a second goroutine receives them, the values are received in the
// order sent.

// -----------------------------------------------------------------------------
// Making channels

// T is a channel type.

make(T)        // unbuffered channel of type T
make(T, n)     // buffered channel of type T, buffer size n

c := make(chan int, 10)     // channel with a buffer size of 10

// -----------------------------------------------------------------------------
// Send statements

// A send statement sends a value on a channel. The channel expression must be
// of channel type, the channel direction must permit send operations, and the
// type of the value to be sent must be assignable to the channel's element
// type.

// SendStmt = Channel "<-" Expression .
// Channel  = Expression .

// Both the channel and the value expression are evaluated before communication
// begins.

// Communication blocks until the send can proceed.

// A send on an unbuffered channel can proceed if a receiver is ready.

// A send on a closed channel proceeds by causing a run-time panic. A send on a
// nil channel blocks forever.

ch <- 3  // send value 3 on channel ch

// -----------------------------------------------------------------------------
// Receive operator

// For an operand 'ch' of channel type, the value of the receive operation
// '<-ch' is the value received from the channel 'ch'.

// The channel direction must permit receive operations, and the type of the
// receive operation is the element type of the channel.

// The expression blocks until a value is available. Receiving from a nil
// channel blocks forever.

// A receive operation on a closed channel can always proceed immediately,
// yielding the element type's zero value after any previously send values
// have been received.

v1 := <-ch
v2 = <-ch
f(<-ch)
<-strobe   // wait until clock pulse and discard received value

// A receive expression used in an assignment or initialization of the form

x, ok = <-ch
x, ok := <-ch
var x, ok = <-ch

// yields an additional result of type bool reporting whether the communication
// succeeded. The value of ok is true if the value received was delivered by a
// successful send operation to the channel, or false if it is a zero value
// generated because the channel is closed and empty.

// -----------------------------------------------------------------------------
// Goroutines

// They're called _goroutines_ because the existing terms--threads, coroutines,
// processes, and so on--convey inaccurate connotations. A goroutine has a
// simple model: it is a function executing concurrently with other goroutines
// in the same address space.

// It is lightweight, costing little more than the allocation of stack space.
// And the stack starts small, so they are cheap, and grow by allocating (and
// freeing) heap storage as required.

// Goroutines are multiplexed onto multiple OS threads so if one should block,
// such as while waiting for I/O, others continue to run. Their design hides
// many of the complexities of thread creation and managmenet.

// Prefix a function or method call with the 'go' keyword to run the call in a
// new goroutine. When the call completes, the goroutine exits, silently.

go list.Sort()  // run list.Sort concurrently; don't wait for it.

// A function literal can be handy in a goroutine invocation.

func Announce(message string, delay time.Duration) {
	go func() {
		time.Sleep(delay)
		fmt.Println(message)
	}()  // Note the parentheses - must call the function.
}

// In Go, function literals are closures: the implementation makes sure the
// variables referred to by the function survive as long as they are active.

// These examples aren't too practical because the functions have no way of
// signaling completion. For that, we need channels.

// -----------------------------------------------------------------------------
// Channels

// Like maps, channels are allocated with make, and the resulting value acts as
// a reference to an underlying data structure. If an optional integer parameter
// is provided, it sets the buffer size for the channel. The default is zero,
// for an unbuffered or synchronous channel.

ci := make(chan int)            // unbuffered channel of integers
cj := make(chan int, 0)         // unbuffered channel of integers
cs := make(chan *os.File, 100)  // buffered channel of pointers to Files

// Unbuffered channels combine communication--the exchange of a value--with
// synchronization--guaranteeing that two calculations (goroutines) are in a
// known state.

// There are lots of nice idioms using channels. Here's one to get us started.
// In the previous section we launched a sort in the background. A channel can
// allow the launching goroutine to wait for the sort to complete.

c := make(chan int)

// Start the sort in a goroutine; when it completes, signal on the channel.
go func() {
	list.Sort()
	c <- 1  // Send a signal; value diesn't maktter.
}()
doSomethingForAWhile()
<-c  // Wait for sort to finish; discard sent value.

// Receivers always block until there is data to receive.

// If the channel is unbuffered, the sender blocks until the receiver has
// received the value.

// If the channel has a buffer, the sender blocks only until the value has been
// copied to the buffer; if the buffer is full, this means waiting until some
// receiver has retrieved a value.

// A buffered channel can be used like a semaphore, for instance to limit
// throughput.

// In this example, incoming requests are passed to handle, which sends a value
// into the channel, processes the request, and then receives a value from the
// channel to ready the 'semaphore' for the next consumer. The capacity of the
// channel buffer limits the number of simultaneous calls to process.

var sem = make(chan int, MaxOutstanding)

func handler(r *Request) {
	sem <- 1    // Wait for active queue to drain.   // Put something in the buffer, if full, block.
	process(r)  // May take a long time.             // DoWork()
	<-sem       // Done; enable next request to run. // Take something out of the buffer.
}

func Serve(queue chan *Request) {
	for {
		req := <-queue
		go handle(req)  // Don't wait for handle to finish.
	}
}

// Once MaxOutstanding handlers are executing process, any more will block
// trying to send into the filled channel buffer, until one of the existing
// handlers finishes and receives from the buffer.

// This design has a problem though: Serve creates a new goroutine for every
// incoming request, even though only MaxOutstanding of them can run at any
// moment.

// As a result, the program can consume ulimited resources if the requests
// come in too fast. We can address that deficiency by changing Serve to gate
// the creation of the goroutines.

// Here's an obvious solution, but beware it has a bug we'll fix subsequently:

func Serve(queue chan *Request) {
	for req := range queue {
		sem <- 1
		go func() {
			process(req)  // Buggy, see below.
			<-sem
		}()
	}
}

// The bug is that in a Go for loop, the loop variable is reused for each
// iteration, so the req variable is shared across all goroutines. That's not
// what we want. We need to make sure that req is unique for each goroutine.
// Here's one way to do that, passing the value of req as an argument to the
// closure in the goroutine:

func Serve(queue chan *Request) {
	for req := range queue {
		sem <- 1
		go func(req *Request) {
			process(req)
			<-sem
		}(req)
	}
}

// Another:

func Serve(queue chan *Request) {
	for req := range queue {
		req := req  // Create new instance of req for the goroutine.
		sem <- 1
		go func() {
			process(req)
			<-sem
		}()
	}
}

// You get a fresh version of the variable with the same name, deliberately
// shadowing the loop variable locally but unique to each goroutine.

// Going back to the general problem of writing the server, another approach
// that manages resources well is to start a fixed number of handle goroutines
// all reading from the request channel.

// The number of goroutines limits the number of simultaneous calls to process.
// This Serve function also accepts a channel on which it will be told to exit;
// after launching the goroutines it blocks receiving from that channel.

func handle(queue chan *Request) {
	for r := range queue {
		process(r)
	}
}

// for channels, the iteration values produced are the successive values sent
// on the channel until the channel is closed. If the the channel is nil, the
// range expression blocks forever.

func Serve(clientRequests chan *Request, quit chan bool) {
	// Start handlers
	for i := 0; i < MaxOutstanding; i++ {
		go handle(clientRequests)
	}
	<-quit  // Wait to be told to exit.
}

// BB: Why block on Serve after firing off concurrent handlers? Why not, call it
// FireUpTheHandlers and just return?

// -----------------------------------------------------------------------------
// Channels of channels

// One of the most important properties of Go is that a channel is a first-class
// value that can be allocated and passed around like any other. A common use of
// this property is to implement safe, parallel demultiplexing.

// In the exmaple in te previous section, handle was an idealized handler for a
// request but we didn't define the type it was handling.

// If that type includes a channel on which to reply, each client can provide
// its own path for the answer. Here's a schematic definition of type Request.

type Request struct {
	args        []int
	f           func([]int) int
	resultChan  chan int
}

// The client provides a function and its arguments, as well as a channel inside
// the request object on which to receive the answer.

func sum(a []int) (s int) {
	for _, v := range a {
		s += v
	}
	return
}

request := &Request{[]int{1,2,3}, sum, make(chan int)}

// Send request.
clientRequests <- request

// Wait for response.
fmt.Printf("answer: %d\n", <-request.resultChan)

// On the server side, the handler function is the only thing that changes.

func handle(queue chan *Request) {
	for req := range queue {
		req.resultChan <- req.f(req.args)
	}
}

// There's clearly a lot more to do to make it realistic, but this code is a
// framework for a rate-limited, parallel, non-blocking RPC system, and there's
// not a mutex in sight.

// -----------------------------------------------------------------------------
// Parallelization

// Another application of these ideas is to parallelize a calculation across
// multiple CPU cores. If the calculation can be broken into separate pieces
// that can execute independently, it can be parallelized, with a channel to
// signal when each piece completes.

// Lets say we have an expensive operation to perform on a vector of items, and
// that the value of the operation on each item is independent, as in this
// idealized example.

type Vector []float64

// Apply the operation to v[i], v[i+1] ... up to v[n-1].
func (v Vector) DoSome(i, n int, u Vector, c chan int) {
	for ; i <  n ; i++ {
		v[i] += u.Op(v[i])
	}
	c <- 1  // signal that this piece is done
}

// We launch the pieces independently in a loop, one per CPU. They can complete
// in any order but it doesn't matter, we just count the completion signals by
// draining the channel after launching all the goroutines.

const NCPU = 4  // number of CPU cores

func (v Vector) DoAll(u Vector) {
	c := make(chan int, NCPU)      // Buffering optional but sensible.
	for i := 0; i < NCPU; i++ {
		go v.DoSome(i*len(v)/NCPU, (i+1)*len(v)/NCPU, u, c)
	}
	// Drain the channel.
	for i := 0; i < NCPU; i++ {
		<-c   // wait for one task to complete
	}
	// All done.
}

// The current implementation of the Go runtime will not parallelize this code
// by default. It dedicates only a single core to user-level processing. An
// arbitrary number of goroutines can be blocked in system calls, but by default
// only one can be executing user-level code at any time.

// It should be smarter and one day it will be smarter, but until it is if you
// want CPU parallelism you must tell the run-time how many goroutines you want
// executing code simultaneously. [..]

// Be sure not to confuse the ideas of concurrency--structuring a program as
// independently executing components--and parallelism--executing calculations
// in parallel for efficiency on multiple CPUs. Although the concurrency
// features of Go can make some problems easy to structure as parallel
// computations, Go is a concurrent language, not a parallel one, and not all
// parallelization problems fit Go's model.

// -----------------------------------------------------------------------------
// A leaky buffer

// The tools of concurrent programming can even make non-concurrent ideas easier
// to express.

// Here's an example abstracted from an RPC package.

// The client goroutine loops receiving data from some source, perhaps a
// network. To avoid allocating and freeing buffers, it keeps a free list, and
// uses a buffered channel to represent it. If the channel is empty, a new
// buffer gets allocated. Once the message buffer is ready, it's sent to the
// server on serverChan.

var freeList = make(chan *Buffer, 100)
var serverChan = make(chan *Buffer)

func client() {
	for {
		var b *Buffer
		// Grab a buffer if available; allocate if not.
		select {
		case b = <-freeList:
			// Got one; nothing more to do.
		default:
			// None free, so allocate a new one.
			b = new(Buffer)
		}
		load(b)          // Read next message from the net.
		serverChan <- b  // Send to server.
	}
}

// The server loop receives each message from the client, processes it, and
// returns the buffer to the free list.

func server() {
	for {
		b := <-serverChan    // Wait for work.
		process(b)
		// Reuse buffer if there's room.
		select {
		case freeList <- b:
			// Buffer on free list; nothing more to do.
		default:
			// Free list full, just carry on.
		}
	}
}

// The client attempts to retrieve a buffer from freeList; if none is
// available, it allocates a fresh one. The server's send to freeList puts b
// back on the free list unless the list is full, in which case the buffer is
// dropped on the floor to be reclaimed by the gc.

// Notice how select never blocks.

// This implementation builds a leaky bucket free list in just a few lines,
// relying on the buffered channel and the gc for bookkeeping.
