// Some examples taken from:
// http://chrismorgan.info/blog/rust-ownership-the-hard-way.html 

// example1.rs
// Author:        mike mcgirr
// Compile with:  $rustc example0.rs


// By default struct `A` has what's called `move semantics`
// However let's derive `Copy` and `Clone` for struct `A` so it can have `copy semantics`
#[derive(Clone, Copy)]
struct A;

fn main() 
{
    // We will now instantiate the struct A in a variable
    let a = A;
    
    // Now we are cloning and copying that instance of the `A` struct 
    // to the variable `b`
    let b = a; 
    
    let c = a; 
}
