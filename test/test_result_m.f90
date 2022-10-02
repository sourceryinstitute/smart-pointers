module test_result_m
  !! Define a basic abstraction for describe test intentions and results
  implicit none

  private
  public :: test_result_t

  type test_result_t
    private
    character(len=:), allocatable :: description_
    logical outcome_
  contains
    procedure :: characterize
  end type

  interface test_result_t

    pure module function construct(description, outcome) result(test_result)
      implicit none
      character(len=*), intent(in) :: description
      logical, intent(in) :: outcome
      type(test_result_t) test_result 
    end function

  end interface

  interface

    pure module function characterize(self) result(characterization)
      implicit none
      class(test_result_t), intent(in) :: self
      character(len=:), allocatable :: characterization
    end function

  end interface

end module test_result_m

submodule(test_result_m) test_result_s
  implicit none

contains

    module procedure construct
      test_result%description_ = description
      test_result%outcome_ = outcome
    end procedure

    module procedure characterize
      characterization = merge("Pass: ", "Fail: ", self%outcome_) // self%description_
    end procedure

end submodule test_result_s
