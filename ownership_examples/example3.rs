struct Person 
{  
    name: String,
    age: u32
}

fn hello() 
{  
    let person = Person {
        name: "Mike McGirr".to_string(),
        age: 24
    };

    // lend the person -- don't transfer ownership
    let twenties = is_in_twenties(&person);
    // take the person -- transfer ownership
    //let twenties = is_in_twenties_take_ownership(person);

    // now this scope still owns the person unless we used the take_ownership version
    println!("{:?}, twenties: {:?}", person.name, twenties);
}

// Borrow the person variable passed in
fn is_in_twenties(person: &Person) -> bool
{
    person.age >= 20 && person.age < 30
}

// Take ownership of the person variable passed in
fn is_in_twenties_take_ownership(person: Person) -> bool
{
    person.age >= 20 && person.age < 30
}

fn main() 
{
    hello();
}
