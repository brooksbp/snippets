fn main() {
    println!("Hello, world!");
    println!(" {} ", column!());
    assert!((column!() == 4));
}
