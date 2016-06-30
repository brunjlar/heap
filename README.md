# heap
## Dependently typed Haskell heaps

This is my first attempt to do "real" *dependently typed programming* in Haskell:
An implementation of *leftists heaps* (following [Chris Okasaki's "Purely Functional Data Structures"]
(http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/purely-functional-data-structures))
where both the *heap invariant* and the *leftist property* are statically ensured by the type system.

To encode those invariants, I abstract the concept of *type-level natural number* into a type class, of which I provide two implementations:
- simple, *unary* "Peano" numbers, which are of course highly inefficient, but easier to understand and
- *binary* numbers, which are more efficient, but also more complicated.

Using the binary encoding of type-level natural numbers, I can quickly sort a list of 50000 elements.
