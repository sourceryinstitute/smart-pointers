Test Support
============

This directory exists to support the test suite: programs compiled from source
code contained here can be executed by a test using Fortran's `execute_command_line`
subroutine.  Doing so facilitates checking for expected error termination without
actually terminating the execution of the test suite.
