module assert_m
  !! Enforce logical assertions that can be toggled on/off at compile-time
  !! To turn off assertions, building with the flag -DUSE_ASSERTIONS=.false.
  implicit none

  private
  public :: assert

#ifndef USE_ASSERTIONS
# define USE_ASSERTIONS .true.
#endif
  logical, parameter :: enforce_assertions = USE_ASSERTIONS

  interface 

     pure module subroutine assert(assertion, description)
       !! Error terminate on .false. assertion with the stop code given by description
       !! With IBM XL Fortran, the stop code is an integer due to character stop codes being unsupported.
       implicit none
       logical, intent(in) :: assertion
       character(len=*), intent(in) :: description
     end subroutine

  end interface

end module assert_m
