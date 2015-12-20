/* 
 * channels.rs
 * author: mike mcgirr
 * 
 * This example will create a shared channel that can be sent 
 * along from many threads.
 * Rust's channels can be used to communicated both messages and ownership
 * between threads.
 *
 * An excerpt from the Rust Programming Languages blog:
 *
 *   "A channel transfers ownership of the messages sent along it, 
 *    so you can send a pointer from one thread to another without 
 *    fear of the threads later racing for access through that pointer. 
 *    Rust’s channels enforce thread isolation."
 *
 *  http://blog.rust-lang.org/2015/04/10/Fearless-Concurrency.html 
 *   
 */


use std::thread;
use std::sync::mpsc::channel;

fn main() 
{
    // tx is the sending half (tx for transmission)
    // rx is the receiving half (rx for receiving)
    let (tx, rx) = channel();
    
    // create ten threads
    for i in 0..10 
    {
        let tx = tx.clone();
        
        thread::spawn(
            move|| 
            {
                tx.send(
                    format!("This is the {} thread\n", i) 
                ).unwrap();
            }
        );
    }

    for _ in 0..10 
    {
        // receive the threads through the receiving half of the channel.
        let j = rx.recv().unwrap();
        
        print!("{}", j);
    }
}
