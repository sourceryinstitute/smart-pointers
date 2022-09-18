Compiler Support Status
=======================

This directory contains two categories of unit tests separated into two files:

* `usage_test.f90` exercises the reference-counter library, whereas
* `compiler_test.f90` tests the compiler without using reference-counter, 

`compiler_test.f90` verifies that a compiler calls a type's final subroutine
in each of the scenarios in which the Fortran standard requires finalization
to occur.

`nagfor` 7.1.0 
--------------
:trophy: The Numerical Algorithms Group (NAG) Fortran [compiler] passes all
reference-counter tests. 

`gfortran` 12.2.0
-----------------
Because the first usage test listed below causes a segmentation fault,
obtaining the `gfortran` test results requires skipping that test by, 
for example, running running individual tests as follows:
```
fpm test -- -f "<description>"
```
Replace <description> above with one of the enumerated test descriptions
below or with a corresponding substring not contained in the first usage 
test description.

### Failing checks in `compiler_test.f90`
1. finalizes a non-allocatable object on the LHS of an intrinsic assignment
2. finalizes an allocated allocatable LHS of an intrinsic assignment
3. finalizes a function reference on the RHS of an intrinsic assignment
4. finalizes a specification expression function result

### Failing checks in `usage_test.f90`
1. copy points to the same resource as the original
2. has zero references after a shallow copy goes out of scope

[compiler]: https://www.nag.com/content/nag-fortran-compiler
