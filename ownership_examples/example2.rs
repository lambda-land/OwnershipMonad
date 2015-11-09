/* 
 example2.rs
 Author: mike mcgirr
 compile with: $rustc example2.rs

 This example is less to illustrate the ownership concept in Rust
 and instead demonstrates the concept of Traits in Rust 
 and their similarity to Type Classes in Haskell. 

 The Haskell examples are taken from:
 http://learnyouahaskell.com/making-our-own-types-and-typeclasses#typeclasses-102 
*/

// let's enumerate a data type structure and call it `TrafficLight`
enum TrafficLight { 
                    Red,
                    Yellow,
                    Green,
                  }
/* In Haskell the equivalent would be a data type like:

   data TrafficLight = Red 
                     | Yellow
                     | Green
*/

// Let's create a trait called Eq for testing equality of ourself and one other
// type like ourself
trait Eq 
{
    fn is_equal(&self, Self) -> bool;
}

/* 
  Something that implements the Eq trait would need to have a function `is_equal` defined to 
  fully implement that trait.

  `is_equal` is a function that takes a reference to self (hence the &self) 
  and some other struct of the same type and returns a bool.
  
  This is a lot like a type class in Haskell:

   class Eq a where 
        isEqual :: a -> a -> Bool

*/

// Let's implement the Eq trait for TrafficLight
impl Eq for TrafficLight 
{
    fn is_equal(&self, other: TrafficLight ) -> bool 
    {
        match (self, other) // match against a pair expression in this case
        {
            (&TrafficLight::Red,    TrafficLight::Red)     => true,  
            (&TrafficLight::Yellow, TrafficLight::Yellow)  => true,
            (&TrafficLight::Green,  TrafficLight::Green)   => true,
            _                                              => false
        }
    }
}

/* Imagine we couldn't derive Eq for TrafficLight using automatic syntactic sugar
 * like the `#[derive]` attribute.
 *
 * Or that it was something that was not derived for us in the unsolicited manner that 
 * equality is normally for some types.
 * 
 * In this case we have to implement it by hand.
 *
 * This is sort of like writing up an instance by hand in Haskell for making
 * instances of typeclasses.
 *
 * It would be as if we did the following in Haskell
    
    instance Eq TrafficLight where  
        Red == Red = True  
        Green == Green = True  
        Yellow == Yellow = True  
        _ == _ = False  

*/


// Main function for testing the above types
fn main() {

    let light : TrafficLight = TrafficLight::Red;
    let other : TrafficLight = TrafficLight::Red;

    let are_they_equal : bool = light.is_equal(other);

    println!("Are light and other equal? {}.", are_they_equal);
}

