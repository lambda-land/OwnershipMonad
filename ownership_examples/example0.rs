// Some examples taken from:
// http://chrismorgan.info/blog/rust-ownership-the-hard-way.html 

// example0.rs
// Author: mike mcgirr
// Compile with:
// $rustc example0.rs


// In Rust you can you can define a struct with no members at all: 
struct A;
// Such a struct is called ‘unit-like’ because it resembles the empty tuple, (), 
// sometimes called ‘unit’. Anyways it defines a new type. This is useful but in ways 
// that are out of the scope of this example. 

fn main() 
{
    // We will now instantiate the struct A in a variable
    let a = A;
    
    // Rust infered that this means that `a` is of type struct `A`
    // But we could have also said:
    // let a : A = A; 
    
    // Now we are moving the ownership of that instance of the `A` struct 
    // to the variable `b`
    let b = a; 
    
    // For that reason we can not move it into the variable `c`
    // let c = a; 
    // ^^^ comment this out to compile 
}
