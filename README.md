# Reference-Counter
An object-oriented, extensible reference-counting utility for Fortran.

This archive contains the classes presented in the following two papers:

* [Rouson, Xia & Xu (2010)] "Object construction and destruction design
  patterns in Fortran 2003." _Procedia Computer Science_, *1*(1), 1495-1504.
* [Rouson, Morris & Xia (2012)] "This Isn't Your Parents' Fortran: Managing
  C++ Objects with Modern Fortran." _Computing in Science & Engineering_
  *14*(2), 46-54.

This purpose of this repository is to demonstrate and explore solutions to
a minor memory leak that affects only a shadow object's `ref_counter` component,
each instance of which contains only a small amount of information: an
default-integer pointer and polymorphic pointer to an abstratype with a
`ref_counter` as its only component.

[Rouson, Xia & Xu (2010)]: https://doi.org/10.1016/j.procs.2010.04.166
[Rouson, Morris & Xia (2012)]: https://doi.org/10.1109/MCSE.2012.33
