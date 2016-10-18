ORef
====

Mutable references that abide by Affine Logic.


Introduction
------------

STRef has the `readSTRef` function which copies the value in the STRef 
into a pure context within the ST Monad. 
The STRef which the pure value was recently read from can then be used in the 
ST by other functions. These functions are free to write to and modify the value
in the STRef.  Because of this, it's not possible to know after the value is read
from the STRef if the pure value is still the same as the *actual* value in the 
STRef. After a `readSTRef` the subsequent pure value is reflective only of what 
the STRef used to be. 

While constantly reading (copying) the current contents of the STRef into a pure value
before actions are taken based of this value is possible - this seems inefficient.
Moreover it doesn't solve the original issue since we are still only *reasonably* 
sure that the value contained in the STRef is what the recent `readSTRef` says it is.

It could be possible to use `modifySTRef :: STRef s a -> (a -> a) -> ST s ()`
and leave it up to the function be applied to the STRef to determine what the state 
of the STRef. But with this approach we are restricted to operating on the value in
the STRef instead of reading it.

Using a mutable reference that abides by Affine type rules allows us to say with
certainty that the value read from an mutable reference is reflective of the contents
of the resource that the reference refers to. Importantly we are allowed to read and
modify the value in place rather than making numerous copies. We can do this since we 
are bound by the Affine type rules.


Design
------


```haskell
newORef :: a -> Own (ORef a)
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
borrowORef :: ORef a -> (a -> Own b) -> Own (ORef b)
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


```haskell
writeORef :: ORef a -> a -> Own ()
```

Set the value in an ORef to a new value. This can only occur if there are no 
borrowers or if it is the sole borrower of an ORef performing the write.


```haskell
modifyORef :: ORef a -> (a -> a) -> Own ()
```

Apply a function on the value in the ORef. Like `writeORef`, this can only occur if there
are no borrowers or if it is the sole borrower performing the operation.
