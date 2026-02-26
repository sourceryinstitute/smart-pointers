module test_m
  !! Define an abstract test type and test-result template method
  use test_result_m, only : test_result_t
  implicit none

  private
  public :: test_t

  type, abstract :: test_t
  contains
    procedure(subject_interface), nopass, deferred :: subject
    procedure(results_interface), nopass, deferred :: results
    procedure :: report
  end type

  abstract interface

    pure function subject_interface() result(specimen)
      character(len=:), allocatable :: specimen
    end function

    function results_interface() result(test_results)
      import test_result_t
      type(test_result_t), allocatable :: test_results(:)
    end function

  end interface

  interface

    module function report(test) result(failures)
      implicit none
      class(test_t), intent(in) :: test
      integer :: failures
    end function

  end interface

end module test_m

submodule(test_m) test_s
#ifdef XLF
  use test_result_m, only : test_result_t
#endif
  implicit none

contains

  module procedure report
    integer i
    type(test_result_t), allocatable :: test_results(:)

    failures = 0
    print *
    print *, test%subject()

    test_results = test%results()
    do i=1,size(test_results)
      print *,"  ",test_results(i)%characterize()
      if (.not. test_results(i)%outcome()) failures = failures + 1
    end do
  end procedure

end submodule test_s
