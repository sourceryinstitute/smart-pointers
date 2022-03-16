Reference-Counter
=================

Overview
--------
An extensible, object-oriented reference-counting utility for Fortran.

This archive contains the classes presented in [Rouson, Xia & Xu (2010)] and [Rouson, Morris & Xia (2012)] refactored to use use more descriptive naming conventions and to separate interface bodies into modules and procedure definitions into submodules.
The 

Compilers
---------
This library uses features form the Fortran 2018 standard.
### Supported compilers
1. The Numerical Algorithms Group (NAG) compiler and
2. The Intel compiler.
### Unsupported compilers
Bug reports have been submitted to the followint two compiler vendors:
1. The GNU compiler.
2. The Cray compiler.

Downloading, Building, and Testing
----------------------------------
With the NAG compiler installed on Linux, macOS, or Windows Subsystem for Linux, download, build, and test with the following shell commands:
```
git clone git@github.com:sourceryinstitute/reference-counter
cd reference-counter
fpm test --compiler nagfor --flag -fpp
```
All tests should pass with recent versions of the supported compilers.

[Rouson, Xia & Xu (2010)]: https://doi.org/10.1016/j.procs.2010.04.166
[Rouson, Morris & Xia (2012)]: https://doi.org/10.1109/MCSE.2012.33
