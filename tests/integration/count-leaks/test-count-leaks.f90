program main
   use iso_fortran_env   ,only : error_unit ,output_unit
   use iso_c_binding, only : c_double
   use vector_implementation, only: vector 
   use faux_cpp_server , only: num_leaks
   implicit none

      type(vector)::x,v,g                      ! Position,velocity,gravity 
      real(c_double)::t=0.,t_final=1.0,dt=0.5 ! Time, end time, time step
      real(c_double), parameter:: tau_p=0.1    ! Aerodynamic reponse time 
      real(c_double), parameter:: zero=0._c_double,one=1._c_double
      x = vector([zero,zero,zero])           ! Object construction
      call x%force_finalize()                ! Final clean-up

   if (num_leaks()==0) then
      write(output_unit,*) 
      write(output_unit,fmt='(a)') "End Result: TEST PASSED" ;
   else
      write(error_unit,*) 
      write(error_unit,fmt='(a)') "End Result: TEST FAILED" ;
   end if

end program main
