# ORef

Mutable references that abide by Affine Logic.


## Introduction

STRef has the `readSTRef` function which copies the value in the STRef 
into a pure context within the ST Monad. 
The STRef which the pure value was recently read from can then be used in
ST by other functions. These functions are free to write to and modify the value
in the STRef.  Because of this it's not possible to know after a value is read
from an STRef if the pure value the read returns is still the same as the *actual*
value in the STRef.  After a `readSTRef` the subsequent pure value is reflective
only of what the STRef used to be. 

While constantly reading (copying) the current contents of the STRef into a pure value
before actions are taken based of this value is possible - this seems inefficient.
Moreover it doesn't solve the original issue since we are still only *reasonably* 
sure that the value contained in the STRef is what the recent `readSTRef` says it is.

It could be possible to use `modifySTRef :: STRef s a -> (a -> a) -> ST s ()`
and leave it up to the function be applied to the STRef to determine what the state 
of the STRef. But with this approach we are restricted to operating on the value in
the STRef instead of reading it.

Using a mutable reference that abides by Affine type rules allows us to say with
certainty that the value read from a mutable reference is reflective of the contents
of the resource that the reference refers to. Importantly we are allowed to read and
the value in place rather than making copies. We can do this since we are bound by 
the Affine type rules.


## Design


```haskell
newORef :: Typeable a => a -> Own (ORef a)
```

Take a pure value and create an ORef with the value in it.


```haskell
copyORef :: ORef a -> Own (ORef a)
```

Copy the contents of an existing ORef into a separate ORef.
There will be two ORefs after this operation that immediately
have the same value in them.


```haskell
moveORef :: ORef a -> Own (ORef a)
```

Move the "contents" of the older ORef into a newer ORef.
The older ORef can no longer be referred to since the resource inside
it has moved to the new ORef. The new ORef has taken ownership of the
resource.


```haskell
moveORef' :: ORef a -> ORef b -> Own ()
```

Move the contents of one ORef to an existing ORef
~~This will fail if either ORefs have borrowers~~


```haskell
readORef :: Typeable a => ORef a -> (a -> Own b) -> Own b
```

Read an ORef and use it in the given continuation.


```haskell
writeORef :: ORef a -> a -> Own ()
```

Set the value in an ORef to a new value. 
~~This can only occur if there are no borrowers or if it is the sole borrower of an ORef performing the write.~~

-----

### Borrows

**TODO**

Implementing borrows of ORefs will necessitate rewriting the current structure of the Store.

The basic idea behind a borrow is it is a way to create a temporary reference 
to a resource owned by another ORef without taking the resource away permanently 
from the owner. 

A borrow looks like a normal ORef and behave like one (with a few restrictions) but instead of
owning a resource (being bound directly as a variable to a resource) it points at the resource 
owned by another ORef. 

If an ORef is being borrowed it will not be able to mutate itself or go away. For that reason an 
owner ORef has to keep track of whether or not it currently has any borrowers.

A borrower should be able to mutate the resource if it is the only borrower.

**Question**: 
Is a borrow still a necessary component of an ORef as it is currently implemented?

**Answer**:
*I'm still considering this.*
The main reason a borrow **would** be desired would be to make it possible to
use the underlying resource (or even possibly borrow a portion of the resource), 
either to read or to write to it, **without** having to copy it or take ownership 
of it.

But given that under the hood the Haskell runtime **isn't actually making a copy** 
the argument for including borrows loses some of its luster.

But if that were **not** the case, then including borrows would be important to include.

The task then is to find cases where we need to be able to read a value 
and if we were to make a copy to be able to read the value, we are *actually* making a copy. 
A borrow, under these circumstatnces, would allow us to have access without copying or moving 
ownership to ourselves.

I'm more leaning towards **including** it since it would be useful for those latter cases. 
The downside is that it would make using ORef's more complex.

Idris's Uniqueness Types were [inspired by Rust](http://docs.idris-lang.org/en/latest/reference/uniqueness-types.html#uniqueness-types) 
and they [felt that borrows were necessary.](http://docs.idris-lang.org/en/latest/reference/uniqueness-types.html#borrowed-types)



**Previous borrowORef docs**

```haskell
borrowORef :: ORef a -> Own (ORef a)
```

Borrowing an ORef is similar to moving an ORef but the borrower intends to return
the resource back to the original owner.

The borrower only references the ORef it is borrowing - the owner meanwhile guarantees 
to its borrowers that it will not mutate or destroy the resource.

A borrower does not take ownership of the resource and after some operation it will
return the resource back to the original owner.

If there is only one borrower then that *sole* borrower can mutate the value in the
ORef.  If there are more than one borrower for any particular ORef - then these borrowers 
only have read access to resource.

The ORef being borrowed from tracks the number of borrowers it has by incrementing its
count of borrowers.
