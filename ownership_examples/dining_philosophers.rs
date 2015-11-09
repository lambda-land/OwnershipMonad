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

use std::thread;
use std::time::Duration;

struct Philosopher 
{
    name: String,
}

impl Philosopher 
{
    fn new(name: &str) -> Philosopher 
    {
        Philosopher { name: name.to_string(), }
    }

    fn eat(&self) 
    {
        println!("{} is eating.", self.name);
        thread::sleep(Duration::from_millis(1000));
        println!("{} is done eating.", self.name);
    }
}

fn main() {
    let philosophers = vec![
        Philosopher::new("Judith Butler"),
        Philosopher::new("Gilles Deleuze"),
        Philosopher::new("Karl Marx"),
        Philosopher::new("Emma Goldman"),
        Philosopher::new("Michel Foucault"),
    ];
    
    let handles: Vec<_> = philosophers.into_iter().map(|p| {
        thread::spawn(move || {
            p.eat();
        })
    }).collect();

    for h in handles 
    {
        h.join().unwrap();
    }
}
