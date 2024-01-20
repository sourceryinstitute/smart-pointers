Compiler Support Status
=======================

This directory contains two categories of unit tests:

* [compiler_test_m.f90] tests the compiler without using Smart-Pointers, whereas
* [sp_smart_pointer_test_m.f90] exercises the Smart-Pointers library.

[compiler_test_m.f90] verifies that a compiler calls a type's final subroutine
in each of the scenarios in which the Fortran 2018 standard requires finalization
to occur.  

Summary
-------

* [AMD](#amd): unsupported (fails to build Smart-Pointers)
* [Cray](#cray): partial support (3 test failures)
* [GCC](#gcc): full support :trophy: (0 test failures)
* [IBM](#ibm): partial support (1 test failure)
* [Intel](#intel): partial support (1 test failure)
* [LLVM](#llvm): unsupported (fails to build Smart-Pointers)
* [NAG](#nag): full support :trophy: (0 test failures)
* [NVIDIA](#nvidia): partial support (2 test failures)

Detailed Results
----------------

### AMD
- Version tested: 13.0.0 (AOCC_3.2.0-Build#128 2021_11_12)
- Result: Fails to build due to an internal compiler error (ICE)

### Cray
- Version: 13.0.1
- Result: at least 3 test failures.
The Smart-Pointers library does not currenlty build with the Cray Fortran compiler, but
the compiler standard-conformance tests embodied in [compiler_test_m.f90] also exist in
a standalone format in a type-finalization [branch] of the Berkely Lab fork of the
llvm-test-suite repository.  The reslts of running those compiler test show three compiler
test failures as follows:
```
git clone https://github.com/berkeleylab/llvm-test-suite
cd llvm-test-suite/Fortran/UnitTests/finalization
ftn compile_me_only.f90
> ./a.out
 Cray Fortran : Version 13.0.1 fails to compile specification_expression_finalization.f90
 Pass: finalizes a non-allocatable object on the LHS of an intrinsic assignment
 Fail: finalizes an allocated allocatable LHS of an intrinsic assignment
 Pass: finalizes a target when the associated pointer is deallocated
 Pass: finalizes an object upon explicit deallocation
 Pass: finalizes a non-pointer non-allocatable object at the END statement
 Pass: finalizes a non-pointer non-allocatable object at the end of a block construct
 Pass: finalizes a function reference on the RHS of an intrinsic assignment
 Fail: finalizes a specification expression function result
 Fail: finalizes an intent(out) derived type dummy argument
 Pass: finalizes an allocatable component object
```
The number of unit test failures for the remainder of the Smart-Pointers test suite is unknown.

### GCC
- Version: 13.1.0
- Result: 0 test failures.
```
 % fpm test
Project is up to date

 The compiler
   Pass: finalizes a non-allocatable object on the LHS of an intrinsic assignment
   Pass: finalizes an allocated allocatable LHS of an intrinsic assignment
   Pass: finalizes a target when the associated pointer is deallocated
   Pass: finalizes an object upon explicit deallocation
   Pass: finalizes a non-pointer non-allocatable object at the END statement
   Pass: finalizes a non-pointer non-allocatable object at END BLOCK statement
   Pass: finalizes a function reference on the RHS of an intrinsic assignment
   Pass: finalizes a specification expression function result
   Pass: finalizes an intent(out) derived type dummy argument
   Pass: finalizes an allocatable component object

 A smart_pointer
   Pass: creates a resource when constructed
   Pass: removes the resource when the object goes out of scope
   Pass: copy points to the same resource as the original
   Pass: has zero references after a shallow copy goes out of scope
```

The above gfortran version was built from source using commands of the following form:
```
git clone git@github.com:sourceryinstitute/opencoarrays
cd opencoarrays
./install.sh -p gcc -b master -j <num-threads>
export LD_LIBRARY_PATH="${PWD}/prerequisites/installations/lib"
export PATH="${PWD}/prerequisites/installations/bin:$PATH"
```
after replacing `<num-threads>` with the desired number of threads for an accelerated,
multithreaded build.  Producing the above test results requires GCC commit
[d7caf313525a46f200d7f5db1ba893f853774aee], which reduced the number of gfortran test
failures from six to zero.

### IBM
- Version: IBM Open XL Fortran for AIX 17.1.0 
- Result: 1 test failure
```
$ fpm test --archiver ar --compiler xlf2008_r --flag -DXLF
<WARN> Unknown compiler xlf2008_r requested! Defaults for this compiler might be incorrect
Project is up to date
 
 The compiler
   Pass: finalizes a non-allocatable object on the LHS of an intrinsic assignment
   Pass: finalizes an allocated allocatable LHS of an intrinsic assignment
   Pass: finalizes a target when the associated pointer is deallocated
   Pass: finalizes an object upon explicit deallocation
   Pass: finalizes a non-pointer non-allocatable object at the END statement
   Pass: finalizes a non-pointer non-allocatable object at END BLOCK statement
   Pass: finalizes a function reference on the RHS of an intrinsic assignment
   Pass: finalizes a specification expression function result
   Pass: finalizes an intent(out) derived type dummy argument
   Pass: finalizes an allocatable component object
 
 A smart_pointer
   Pass: creates a resource when constructed
   Pass: removes the resource when the object goes out of scope
   Pass: copy points to the same resource as the original
   Fail: has zero references after a shallow copy goes out of scope

```

### Intel
- Versions: 
   - `ifx` 2024.0.0 Build 20231017
   - `ifort` 2021.7.0 Build 20220726\_000000
- Result: 1 test failure.
```
 % fpm test --compiler ifort
Project is up to date
 
 The compiler
   
 Pass: finalizes a non-allocatable object on the LHS of an intrinsic assignment
   Pass: finalizes an allocated allocatable LHS of an intrinsic assignment
   Pass: finalizes a target when the associated pointer is deallocated
   Pass: finalizes an object upon explicit deallocation
   Pass: finalizes a non-pointer non-allocatable object at the END statement
   Pass: finalizes a non-pointer non-allocatable object at END BLOCK statement
   Pass: finalizes a function reference on the RHS of an intrinsic assignment
   Pass: finalizes a specification expression function result
   Pass: finalizes an intent(out) derived type dummy argument
   Pass: finalizes an allocatable component object
 
 A smart_pointer
   Pass: creates a resource when constructed
   Pass: removes the resource when the object goes out of scope
   Pass: copy points to the same resource as the original
   Fail: has zero references after a shallow copy goes out of scope
```

### LLVM
- Version tested: `git` commit `76911b5f75907eef53a30cc3` (9/20/2022)
- Result: Fails with the error message `not yet implemented: derived type finalization`

### NAG 
- Version: 7.1 (Build 7113)
- Result: 0 test failures. :trophy:
```
 % fpm test --compiler nagfor --flag -fpp
Project is up to date

 The compiler
   Pass: finalizes a non-allocatable object on the LHS of an intrinsic assignment
   Pass: finalizes an allocated allocatable LHS of an intrinsic assignment
   Pass: finalizes a target when the associated pointer is deallocated
   Pass: finalizes an object upon explicit deallocation
   Pass: finalizes a non-pointer non-allocatable object at the END statement
   Pass: finalizes a non-pointer non-allocatable object at END BLOCK statement
   Pass: finalizes a function reference on the RHS of an intrinsic assignment
   Pass: finalizes a specification expression function result
   Pass: finalizes an intent(out) derived type dummy argument
   Pass: finalizes an allocatable component object

 A smart_pointer
   Pass: creates a resource when constructed
   Pass: removes the resource when the object goes out of scope
   Pass: copy points to the same resource as the original
   Pass: has zero references after a shallow copy goes out of scope
```

### NVIDIA
- Version: 22.7-0 64-bit target on x86-64 Linux -tp zen3
- Result: 2 test failures

[specification_expression_finalization.f90]:  ../example/test-support/specification_expression_finalization.f90
[compiler_test_m.f90]:  ./compiler_test_m.f90
[sp_smart_pointer_test_m.f90]:  ./sp_smart_pointer_test_m.F90
[branch]: https://github.com/BerkeleyLab/llvm-test-suite/tree/fortran-type-finalization/Fortran/UnitTests/finalization
[d7caf313525a46f200d7f5db1ba893f853774aee]: https://github.com/gcc-mirror/gcc/commit/5889c7bd46a45dc07ffb77ec0d698e18e0b99840 
