# heap
## Dependently typed Haskell heaps

This is my first attempt to do "real" *dependently typed programming* in Haskell:
An implementation of *leftists heaps* (following [Chris Okasaki's "Purely Functional Data Structures"]
(http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/purely-functional-data-structures))
where both the *heap invariant* and the *leftist property* are statically ensured by the type system.

To encode those invariants, I used *Peano natural numbers*, which are of course highly inefficient.
For an efficient implementation, a *binary* encoding of natural numbers should be employed instead.

I chose this simple encoding to be able to concentrate on my main interest: How to prove simple properties of natural numbers
in Haskell and then use those properties in code to enforce strong invariants.
