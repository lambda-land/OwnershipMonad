extern crate crossbeam;

use crossbeam::scope;

fn print_vec(vec: &Vec<i32>) {
    // the `vec` parameter is part of this scope, so it's owned by `print_vec`

    for i in vec.iter() {
        println!("{}", i)
    }

    // now, `vec` is deallocated
}

fn main() {
    // a vector is not able to be copied by default
    // so we know that unless we copy or clone it 
    // explicity that there will only be one.
    let mut data = vec![1, 1, 1];

    crossbeam::scope(|scope| {
        for i in 0..3 {
            
            // this line with `move` would be forcing the closure to take 
            // ownership of its environment
            //
            //scope.spawn(move || {
            scope.spawn(|| {
                print_vec(&data);
            });
        }
    });

    /* work in progress
     *
    // this mutates the values in data but it has to clone them 
    crossbeam::scope(|scope| {
        for i in 0..3 {

            // this line with `move` forces the closure to take 
            // ownership of its environment
            scope.spawn(move || {
                data_clone[0] += 1;
                println!("In thread {} the value of element 0 is {}", i, data_clone[0]);
            });
        }
    });
    */ 
}
