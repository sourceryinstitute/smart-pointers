module compiler_test
  use vegetables, only: result_t, test_item_t, describe, it, assert_equals
  implicit none

  private
  public :: test_ref_reference

  type inner_object_t
    private
    integer dummy
  contains
    final :: count_finalizations
  end type

  type outer_object_t
    private
    type(inner_object_t), allocatable  :: inner_object
  end type 

  interface inner_object_t
    module procedure  construct
  end interface 

  integer :: finalizations = 0
  integer, parameter :: avoid_unused_variable_warning = 1

contains

  function test_ref_reference() result(tests)
    type(test_item_t) :: tests

    tests = & 
      describe( &
        "The compiler", &
        [ it("performs the number of finalizations required by the Fortran standard", check_compiler) &
      ])  
  end function

  function check_compiler() result(result_)
    type(outer_object_t), allocatable  :: outer_object
    type(result_t) result_

    allocate(outer_object)
    call define(outer_object)
    deallocate(outer_object)                            ! finalizes inner_object component
    result_ = assert_equals(3, finalizations, "finalizations==3")

  contains

    subroutine define(outer_object)
      type(outer_object_t), intent(out) :: outer_object ! finalizes inner_object component
      allocate(outer_object%inner_object)
      outer_object%inner_object = inner_object_t()      ! finalizes inner_object_t result
    end subroutine

  end function
  
  function construct() result(inner_object)
    type(inner_object_t) inner_object
    inner_object%dummy = avoid_unused_variable_warning
  end function

  subroutine count_finalizations(self)
    type(inner_object_t), intent(inout) :: self
    finalizations = finalizations + 1 
    self%dummy = avoid_unused_variable_warning
  end subroutine

end module compiler_test
