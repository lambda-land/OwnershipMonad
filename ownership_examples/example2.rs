// example2.rs
// Author: mike mcgirr
// compile with: $rustc example2.rs


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

// Let's create a trait called Eq
trait Eq {
    // Something that implemented the Eq trait would need to 
    // have a function 
    fn is_equal(&self, TrafficLight) -> bool;
}
/* This is a lot like a type class in Haskell 

   class Eq a where 
        isEqual :: a -> a -> Bool

*/

// Let's implement the Eq trait for TrafficLight
impl Eq for TrafficLight {
    fn is_equal(&self, other: TrafficLight ) -> bool {
        match (self, other) 
        {
            (&TrafficLight::Red,    TrafficLight::Red)     => true,
            (&TrafficLight::Yellow, TrafficLight::Yellow)  => true,
            (&TrafficLight::Green,  TrafficLight::Green)   => true,
            _                                              => false
        }
    }
}
/* Imagine we couldn't derive Eq for TrafficLight automatically 
 * we had to implement it by hand.
 *
 * This is sort of like writing up instance by hand in Haskell for making
 * instances of typeclasses
 *
 * It would be as if we did the following in Haskell
    
    instance Eq TrafficLight where  
        Red == Red = True  
        Green == Green = True  
        Yellow == Yellow = True  
        _ == _ = False  

*/

fn main() 
{
    let light : TrafficLight = TrafficLight::Red;
    let other : TrafficLight = TrafficLight::Red;

    let are_they_equal : bool = light.is_equal(other);

    println!("Are light and other equal? {}.", are_they_equal);
}

