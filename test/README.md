Compiler Support Status
=======================

This directory contains two categories of unit tests separated into two files:
* `compiler_test.f90` checks whether a compiler performs finalizes objects in 
  each scenario that the Fortran 2018 standard requires. 
* `usage_test.f90` exercises the reference-counter library.
The compiler tests verify the compiler in isolation, making _no_ use of the
reference-counter library.

`nagfor` 7.1.0
--------------
The Numerical Algorithms Group [(NAG]) Fortran compiler] passes all of the tests in the
reference-counter test suite. :trophy:

`gfortran` 12.2.0
-----------------
Because the first usage test below causes a segmentation fault,
exposing `gfortran`'s test failures requires running each test individually
as follows:
```
fpm test -- -f "<description>"
```
where one must replace <description> above with one of the enumerated
descriptions below (or a corresponding unambiguous substring).

Failing checks in `compiler_test.f90`:
1. finalizes a non-allocatable object on the LHS of an intrinsic assignment
2. finalizes an allocated allocatable LHS of an intrinsic assignment
3. finalizes a function reference on the RHS of an intrinsic assignment
4. finalizes a function reference on the RHS of an intrinsic assignment
5. finalizes a specification expression function result

Failing checks in `usage_test.f90`:
1. copy points to the same resource as the original
2. has zero references after a shallow copy goes out of scope

[(NAG) Fortran compiler]: https://www.nag.com/content/nag-fortran-compiler

