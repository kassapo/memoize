## Questions

How do I update a a shared object that is accessed by multiple threads in
Haskell? What role do strict and lazy play?

## A first example

To memoize a pure function a cache is created that stores a mapping of function
argument to return value. The cache is shared by all invokers of the wrapped
function and should call the wrapped function only once for each distinct
argument.

Using strict `Data.HashMap.Strict` and strict `atomicModifyIORef'`.


