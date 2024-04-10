Smart Pointers
==============

```
  _________                      __                 
 /   _____/ _____ _____ ________/  |_               
 \_____  \ /     \\__  \\_  __ \   __\              
 /        \  Y Y  \/ __ \|  | \/|  |                
/_______  /__|_|  (____  /__|   |__|                
        \/      \/     \/                           
__________      .__        __                       
\______   \____ |__| _____/  |_  ___________  ______
 |     ___/  _ \|  |/    \   __\/ __ \_  __ \/  ___/
 |    |  (  <_> )  |   |  \  | \  ___/|  | \/\___ \ 
 |____|   \____/|__|___|  /__|  \___  >__|  /____  >
                        \/          \/           \/ 
                             \/            
```

Overview
--------
The Smart-Pointers library tracks references to program resources and automates
the freeing of those resources if and only if the reference count drops to zero.
Most commonly, the reference is a pointer and the resource is memory.  In that
context, Smart-Pointers help to prevent memory leaks and dangling pointers, which
commonly causes programs to crash due to memory limitations or segmentation faults, 
respectively.

To use Smart-Pointers, define a non-abstract derived type that 

1. Extends Smart-Pointer's `sp_smart_pointer_t` type,
2. Implements the inherited `free` deferred binding, and
3. Invokes the inherited `start_count` procedure inside object constructors.

You can then use intrinsic assignments to copy instances of a `sp_smart_pointer_t`
child type, resulting in a [shallow copy] with the advantage that the target
will be finalized only when it becomes safe to do so.  

Example
-------
See the [example](./example) folder for a demonstration of the use of Smart-Pointers.

Background
----------

For more background on the design philosophy and the internal mechanics of Smart
Pointers, see Rouson et al. (see [[1]], [[2]], [[3]]).  This repository's code
originated from refactoring the code in those publications to use more up-to-date
coding conventions.  For example, this repository separates interface bodies
into modules and procedure definitions into submodules.  This repository also
uses more descriptive nomenclature for the types and procedures.

This repository also adds
1. A [Fortran Package Manager] build system,
2. Tests based on the [Sourcery] library's unit-testing utility,
3. Documentation generated by [`ford`] and deployed to the web via GitHub Actions, and
4. Quality control via continuous integration testing using GitHub Actions.

Supported Compilers
-------------------
Correct execution of the Smart-Pointers library depends critically on comprehensive
compiler support for type finalization.  The unit test suite verifies the relevant
compiler standard-conformance, including a test for each scenario in which the
the Fortran 2023 standard requires that an object be finalized.  The following compilers
pass all Smart-Pointers tests:

| _Compiler_              | _Test failures_ | _Version tested_             |
| :---                    | :---            | :---                         |
| GCC `gfortran` :trophy: | 0               | 13.1.0                       |
| LLVM `flang` :trophy:   | 0               | 19.0.0 `git` commit 325f5123 |
| NAG `nagfor` :trophy:   | 0               | 7.1 Build 7113               |

Partially Supported Compilers
-----------------------------
The following compilers pass _most_ smart pointers tests:

| _Compiler_              | _Test failures_ | _Version tested_               |
| :---                    | :---:            | :---                          |
| Cray `ftn`              | 4               | 17.0.0                         |
| IBM `xlf2008_r`         | 1               | 17.1.0 on AIX                  |
| Intel `ifort`           | 1               | 2021.7.0 Build 20220726_000000 |
| Intel `ifx`             | 1               | 2024.0.0 Build 20231017        |
| NVIDIA `nvfortran`      | 2               | 22.7-0                         |

Unsupported Compiler
--------------------
The following compiler fails to build Smart-Pointers due to an internal compiler error (ICE):

| _Compiler_  | _Test failures_ | _Version tested_                            |
| :---        |       :---:     | :---                                        |
| AMD `flang` | N.A. (see Note) | 13.0.0 (AOCC_3.2.0-Build\#128 2021\_11\_12) |

See the [test suite README.md](./test/README.md) for more details on each compiler's test
results.

Downloading, Building, and Testing
----------------------------------
On Linux, macOS, or Windows Subsystem for Linux, download, build, and test with
the following shell commands:
```
git clone https://github.com/sourceryinstitute/smart-pointer
cd smart-pointer
```
followed by one of the commands below corresponding to your compiler choice.

### Fully supported compilers
The following compilers pass all Smart-Pointers tests.

#### GCC `gfortran`
```
fpm test
```

#### LLVM `flang`
```
fpm test --compiler flang-new
```

#### NAG `nagfor`
```
fpm test --compiler nagfor --flag -fpp
```

### Partially supported compilers
The following compilers pass most Smart-Pointers tests.

#### HPE: Cray Compiler Environment (CCE)
Building with `fpm` using the CCE `ftn` compiler wrapper requires an additional
wrapper to identify the wrapped compiler.  Place a file named `crayftn.sh` at the
front of your `PATH` environment variable containing the following contents:
```
#!/bin/bash

ftn "$@"
```
Then test with the following command
```
fpm test --compiler crayftn.sh
```

#### IBM
```
fpm test --archiver ar --compiler xlf2008_r --flag -DXLF
```

#### Intel `ifort`
```
fpm test --compiler ifort
```

#### Intel `ifx`
```
fpm test --compiler ifx --flag "-check all,nouninit"
```
where `fpm` 0.10.0 or later is required and the `--flag` argument is required to circumvent what appears to be a bug in `ifx`'s Memory Sanitizer.

#### NVIDIA `nvfortran`
```
fpm test --compiler nvfortran --flag -Mpreprocess
```

### Unsupported compiler
The following compiler cannot build the Smart-Pointers library.

#### AMD `flang`
```
fpm test --compiler flang --flag -cpp
```

Documentation
-------------
See the Smart-Pointers GitHub Pages [site] for HTML documentation generated with [`ford`].
See the [doc/] subdirectory for a [PlantUML] script that generates the Unified Modeling Language (UML) 
class diagram below of the three derived types in Smart-Pointers.

[1]: https://doi.org/10.1016/j.procs.2010.04.166
[2]: https://doi.org/10.1017/cbo9780511977381 
[3]: https://doi.org/10.1109/MCSE.2012.33
[Fortran Package Manager]: https://github.com/fortran-lang/fpm
[Veggies]: https://gitlab.com/everythingfunctional/veggies
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[site]: https://sourceryinstitute.github.io/smart-pointers
[Atom]: https://atom.io
[PlantUML]: https://plantuml.com
[doc/]: ./doc
[shallow copy]: https://en.wikipedia.org/wiki/Object_copying#Shallow_copy
[767]: https://github.com/fortran-lang/fpm/issues/767
[test/README.md#cray]: ./test/README.md#cray
[Sourcery]: https://github.com/sourceryinstitute/sourcery
[./tests/compiler_test_m.F90]: ./tests/compiler_test_m.F90
[llvm-test-suite]: https://github.com/llvm/llvm-test-suite
