module compiler_test
  use vegetables, only: result_t, test_item_t, describe, it, assert_equals
  implicit none

  private
  public :: test_ref_reference

  type object_t
    private
    integer dummy
  contains
    final :: count_finalizations
  end type

  type wrapper_t
    private
    type(object_t), allocatable  :: object
  end type 

  interface object_t
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
         ,it("finalizes an allocatable component object", check_allocatable_component_finalization) &
      ])  
  end function

  function construct() result(object)
    type(object_t) object
    object%dummy = avoid_unused_variable_warning
  end function

  subroutine count_finalizations(self)
    type(object_t), intent(inout) :: self
    finalizations = finalizations + 1 
    self%dummy = avoid_unused_variable_warning
  end subroutine

  function check_rhs_function_reference() result(result_)
    type(object_t), allocatable  :: object
    type(result_t) result_
    integer initial_tally, delta

    initial_tally = finalizations
    object = object_t() ! finalizes object_t result
    delta = finalizations - initial_tally
    result_ = assert_equals(1, delta)
  end function

  function check_finalize_on_deallocate() result(result_)
    type(object_t), allocatable  :: object
    type(result_t) result_
    integer initial_tally

    initial_tally = finalizations
    allocate(object)
    object%dummy = 1
    deallocate(object)          ! finalizes object
    associate(final_tally => finalizations - initial_tally)
      result_ = assert_equals(1, final_tally)
    end associate
  end function

  function check_intent_out_finalization() result(result_)
    type(result_t) result_
    type(object_t) object
    integer initial_tally

    initial_tally = finalizations
    call finalize_intent_out_arg(object)
    result_ = assert_equals(initial_tally+1, finalizations)

  contains

    subroutine finalize_intent_out_arg(output)
      type(object_t), intent(out) :: output ! finalizes output
      output%dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_allocatable_component_finalization() result(result_)
    type(wrapper_t), allocatable  :: wrapper
    type(result_t) result_
    integer initial_tally, delta

    initial_tally = finalizations

    allocate(wrapper)
    allocate(wrapper%object)
    call finalize_intent_out_component(wrapper)
    delta = finalizations - initial_tally
    result_ = assert_equals(1, delta)

  contains

    subroutine finalize_intent_out_component(output)
      type(wrapper_t), intent(out) :: output ! finalizes object component
      allocate(output%object)
      output%object%dummy = avoid_unused_variable_warning
    end subroutine

  end function
  
end module compiler_test
