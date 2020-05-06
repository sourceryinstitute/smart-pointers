program main
   use iso_c_binding, only : c_double
   use vector_implementation, only : vector
   use faux_cpp_server, only : num_leaks
   implicit none

   type(vector) x
   real(c_double), parameter :: zero=0._c_double
   x = vector([zero,zero,zero])
   call x%force_finalize()

   associate(leaks => num_leaks())
     if (leaks /= 0) then
       print *,"number of leaks: ",leaks
       error stop "Test failed."
     end if
   end associate

   print *,"Test passed."
end program
