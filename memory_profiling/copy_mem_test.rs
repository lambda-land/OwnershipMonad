use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

fn main() {
    let data = Arc::new(Mutex::new(vec![1, 1, 1]));

    for i in 0..3 {
        // clone data
        let data_clone = data.clone();
        
        thread::spawn(move || {
            let mut mut_data = data_clone.lock().unwrap();
            
            //change the number in element 0 each time
            mut_data[0] += 1;
            println!("In thread {} the value of element 0 is {}", i, mut_data[0]);
        });
    }

    thread::sleep(Duration::from_millis(50));
}

/* 
 * One of the results of this program is:
    
    In thread 2 the value of element 0 is 2
    In thread 1 the value of element 0 is 3
    In thread 0 the value of element 0 is 4
*/
