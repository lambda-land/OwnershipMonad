use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

fn main() {
    let data = Arc::new(Mutex::new([1, 1, 1]));

    for i in 0..3 {
        let data_clone = data.clone();
        
        thread::spawn(move || {
            let mut mut_data = data_clone.lock().unwrap();
            
            mut_data[i] += 1;
            println!("element: {} is {}", i, mut_data[i]);
        });
    }

    thread::sleep(Duration::from_millis(50));
}
