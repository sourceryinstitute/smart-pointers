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
        [ it("finalizes an intent(out) derived type dummy argument", check_intent_out_finalization) &
         ,it("finalizes an object upon explicit deallocation", check_finalize_on_deallocate) &
         ,it("finalizes a function reference on the RHS of an intrinsic assignment", check_rhs_function_reference) &
         ,it("finalizes an allocatable component object", check_allocatable_component) &
         ,it("finalizes an object deallocated inside an associate block", check_finalize_inside_associate) &
      ])  
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

  function check_rhs_function_reference() result(result_)
    type(inner_object_t), allocatable  :: inner_object
    type(result_t) result_
    integer initial_tally, delta

    initial_tally = finalizations
    inner_object = inner_object_t() ! finalizes inner_object_t result
    delta = finalizations - initial_tally
    result_ = assert_equals(1, delta)
  end function

  function check_finalize_on_deallocate() result(result_)
    type(inner_object_t), allocatable  :: inner_object
    type(result_t) result_
    integer initial_tally

    initial_tally = finalizations
    allocate(inner_object)
    deallocate(inner_object)          ! finalizes inner_object
    associate(final_tally => finalizations - initial_tally)
      result_ = assert_equals(1, final_tally)
    end associate
  end function

  function check_finalize_inside_associate() result(result_)
    type(inner_object_t), allocatable  :: inner_object
    type(result_t) result_
    integer initial_tally

    associate(initial_tally => finalizations)
      allocate(inner_object)
      inner_object%dummy = 1
      deallocate(inner_object)          ! finalizes inner_object
      associate(delta => finalizations - initial_tally)
        result_ = assert_equals(1, delta)
      end associate
    end associate
  end function

  function check_intent_out_finalization() result(result_)
    type(result_t) result_
    type(inner_object_t) inner_object
    integer initial_tally

    initial_tally = finalizations
    call finalize_intent_out_arg(inner_object)
    result_ = assert_equals(initial_tally+1, finalizations)

  contains

    subroutine finalize_intent_out_arg(output)
      type(inner_object_t), intent(out) :: output ! finalizes output
      output%dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_allocatable_component() result(result_)
    type(outer_object_t), allocatable  :: outer_object
    type(result_t) result_
    integer initial_tally, delta

    initial_tally = finalizations

    allocate(outer_object)
    call finalize_intent_out_component(outer_object)
    delta = finalizations - initial_tally
    result_ = assert_equals(1, delta)

  contains

    subroutine finalize_intent_out_component(output)
      type(outer_object_t), intent(out) :: output ! finalizes inner_object component
      allocate(output%inner_object)
      output%inner_object%dummy = avoid_unused_variable_warning
    end subroutine

  end function
  
end module compiler_test
