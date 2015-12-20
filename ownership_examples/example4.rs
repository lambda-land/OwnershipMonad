fn main() {

    let mut some_vec = vec![1,2,3];

    { 
        // witthin this scope lets 
        // borrow ownership of `some_vec` 
        // in a way that we mutate it:
        //
        let borrowed_vec = &mut some_vec;

        // change something in the borrowed vector
        //
        borrowed_vec[0] = 42; // change the first element to 42 
        borrowed_vec.remove(1); // remove the 2nd element
    }

    // each iterator only gets a borrow of the vector
    for i in some_vec 
    { 
        // now print the contents of the vector
        println!("{}", i); 
    }

}
