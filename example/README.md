Smart-Pointers Example
======================

The [user_object_smart_pointer.f90] demonstrates the use of the
Smart-Pointers library.  The file contains 

* A module that defines a `user_object_t`  and `user_object_ptr_t` types,
* A submodule defining a constructor funciton and a `free` final subroutin,
* A main program with a `block` construct that forces finalization of
  the `user_object` entity declared in the main program.
 
This example exhibits several important subtleties:

1. Smart-Pointers automate object finalization, eliminating the need
   for `allocatable` objects.  
2. The main program source-allocates a raw `user_object` pointer and
   then passes the pointer to a `user_object_ptr_t()` constructor.
3. The `user_object_ptr_t()` constructor nullifies the received pointer
   to encourage the intended practice in which all pointers associated
   with the object are reference-counted pointers.
4. All assignments in the main program and its internal subroutine 
   perform shallow copies, thereby creating new references to one object 
   without copying the object.

Running the example with the following command:
```
fpm run --example user_object_smart_pointer --compiler nagfor --flag -fpp
```
should produce the following output:
```
 Allocating user_object pointer.
 Defining smart_pointer_1.
 Reference count =  1
 Copying smart_pointer_1 into smart_pointer_2.
 Reference count =  2
 Copying smart_pointer_2 into smart_pointer_3.
 Reference count =  3
 smart_pointer_3 going out of scope.
 Reference count =  2
 smart_pointer_1 and smart_pointer_2 going out of scope
 free(): user_object deallocated
```
