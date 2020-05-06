Reference-Counter
=================

Overview
--------
An extensible, object-oriented reference-counting utility for Fortran.

This archive contains the classes presented in the following two papers:

* [Rouson, Xia & Xu (2010)] "Object construction and destruction design
  patterns in Fortran 2003." _Procedia Computer Science_, *1*(1), 1495-1504.
* [Rouson, Morris & Xia (2012)] "This Isn't Your Parents' Fortran: Managing
  C++ Objects with Modern Fortran." _Computing in Science & Engineering_
  *14*(2), 46-54.


Downloading and Building
------------------------
On Linux, macOS, or Windows Subsystem for Linux,
```
git clone git@github.com:sourceryinstitute/reference-counter
mkdir reference-counter/build
cd reference-counter/build
export FC=$(which gfortran)
export CC=$(which gcc)
export CXX=$(which g++)
cmake ..
make
ctest
```

[Rouson, Xia & Xu (2010)]: https://doi.org/10.1016/j.procs.2010.04.166
[Rouson, Morris & Xia (2012)]: https://doi.org/10.1109/MCSE.2012.33
