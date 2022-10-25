# PCF

This project contains several functions that are able to encode and decode all [Primitive Recursive Functions](https://en.wikipedia.org/wiki/Primitive_recursive_function) into natural numbers using [Godel Numbering](https://en.wikipedia.org/wiki/G%C3%B6del_numbering), it stands as a witness that the set of all PCFs are countable and thus enumerable, you can enumerate over the set of natural numbers and for each natural number n, try to decode it to see if it's a valid code for a PCF, this also helps the diagonalization of the argument that not all computable functions are primitive recursive.

This project serve only as a theoretical presentation for some mathematical concepts, actually it can only be used on a very small range of PCFs since Godel Number grows fast, and it will soon take you a whole life long to encode/decode a PCF
