Reference-Counter
=================

Overview
--------
An extensible, object-oriented reference-counting utility for Fortran.

This archive contains the classes presented in [Rouson, Xia & Xu (2010)] and [Rouson, Morris & Xia (2012)] refactored to use use more descriptive naming conventions and to separate interface bodies into modules and procedure definitions into submodules.
The 

Compilers
---------
This library uses features form the Fortran 2018 standard and depends critically on the type finalization rules that became part of Fortran in the 2003 standard.
The Reference-Counter test suite identifies possible type finalization bugs in each of the compilers that we have tested recently.
This README.md document will be updated with additional information after the issues have been reported on the respective compilers.

Downloading, Building, and Testing
----------------------------------
On Linux, macOS, or Windows Subsystem for Linux, download, build, and test with the following shell commands:
```
git clone git@github.com:sourceryinstitute/reference-counter
cd reference-counter
```
followed by one of the commands below depending on your compiler choice.

### GCC (`gfortran`)
```
fpm test
```

### Numerical Algorithms Group (`nagfor`)
```
fpm test --compiler nagfor --flag -fpp
```

### Intel (`ifort`)
```
fpm test --compiler ifort --flag -coarray=shared
```

[Rouson, Xia & Xu (2010)]: https://doi.org/10.1016/j.procs.2010.04.166
[Rouson, Morris & Xia (2012)]: https://doi.org/10.1109/MCSE.2012.33
