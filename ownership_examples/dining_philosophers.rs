/* 
 * dining_philosophers.rs
 * Author: mike mcgirr
 * 
 * This example is taken from section 3.1 of the Rust Book.
 * It depicts a solution to the classic concurrency problem called ‘the dining philosophers’ which 
 * itself was originally conceived by Dijkstra in 1965. 
 * 
 * Available at:
 * https://doc.rust-lang.org/book/dining-philosophers.html 
 * 
 * or alternatively if you have Rust installed on your machine via the rustup script at:
 * file:///usr/local/share/doc/rust/html/book/dining-philosophers.html
 */

use std::thread; // `use` brings names into scope and we're going to need the `thread` module
use std::time::Duration;

struct Philosopher 
{
    name: String,
}

/* This `impl` block lets us define things on the Philosopher struct type.
 * In this case, we define ‘associated functions’ called `new` and `eat` 
 * */
impl Philosopher 
{
    fn new(name: &str) -> Philosopher 
    {
        Philosopher { name: name.to_string(), } // This is the return statement for `new`
    }

    fn eat(&self) 
    {
        println!("{} is eating.", self.name);
        thread::sleep(Duration::from_millis(1000)); 
        // calling `sleep` on the thread will simulate the time it takes to eat
        println!("{} is done eating.", self.name);
    }
}

fn main() {
    // make a vector to store our philosopers
    let philosophers = vec![
        Philosopher::new("Judith Butler"),
        Philosopher::new("Gilles Deleuze"),
        Philosopher::new("Karl Marx"),
        Philosopher::new("Emma Goldman"),
        Philosopher::new("Michel Foucault"),
    ];
    
    // `handles` is a vector of threads
    let handles: Vec<_> = philosophers.into_iter().map(|p| {
        thread::spawn(move || {
            p.eat();
        }) 
    }).collect();
    /*  In last four lines we took our list of philosopers and called `into_iter()` on it.
        This created an iterator that took ownership of each philosopher.

        We then call `map` on that iterator.
        `map` takes a closure as an argument (an anonymous function in Rust)
        and calls that function on each element in turn.

        The closure function that `map` is calling is where the concurrency really happens.
        `thread::spawn` takes a closure as an arg and executes it in a new thread.
        `move` indicates that the closure is taking ownership of the values it is capturing.
        The values that the closure is taking are the `p` variable of the `map` function.
        Note: closures use `||` instead of `()`
        
        Inside of the thead all we need to do is call `eat()` on the philosopher `p`

        `thread::spawn` lacks a semicolon so it's an expression not a statement
        we needed to do this in order to get the correct return type

        We call `collect()` at the very end to make a collection of all the mapped 
        calls. In this case we made a vector. Hence the original `let handles: Vec<_>`
        This had `_` since we will leave it up to Rust to figure out what type the elements
        in this vector are.

        The vector is then made up of the return types of the `thread::spawn` calls

     */

    for h in handles 
    {
        h.join().unwrap();
    }
    /* 
        At the very end we need to loop through the handles for our 
        threads and `join()` them. This simply blocks program execution until 
        the thread has completed execution.
    */
}
