extern crate crossbeam;

use crossbeam::scope;

fn main() {
    let mut data = [1, 1, 1];

    crossbeam::scope(|scope| {
        for i in 0..3 {
            scope.spawn(move || {
                data[i] += 1;
                println!("element: {} is {}", i, data[i]);
            });
        }
    });
}
