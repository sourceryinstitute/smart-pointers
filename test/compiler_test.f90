module compiler_test
  use vegetables, only: result_t, test_item_t, describe, it, assert_equals
  implicit none

  private
  public :: test_ref_reference

  type object_t
    integer dummy
  contains
    final :: count_finalizations
  end type

  type wrapper_t
    private
    type(object_t), allocatable :: object
  end type

  integer :: finalizations = 0
  integer, parameter :: avoid_unused_variable_warning = 1

contains

  function test_ref_reference() result(tests)
    type(test_item_t) tests

    tests = &
      describe( &
        "The compiler", &
        [ it("finalizes a non-allocatable object on the LHS of an intrinsic assignment", check_lhs_object) & 
         ,it("finalizes an allocated allocatable LHS of an intrinsic assignment", check_allocated_allocatable_lhs) &
         ,it("finalizes a target when the associated pointer is deallocated", check_target_deallocation) &
         ,it("finalizes an object upon explicit deallocation", check_finalize_on_deallocate) &
         ,it("finalizes a non-pointer non-allocatable array object at the END statement", check_finalize_on_end) &
         ,it("finalizes a non-pointer non-allocatable object at the end of a block construct", check_block_finalization) &
         ,it("finalizes a function reference on the RHS of an intrinsic assignment", check_rhs_function_reference) &
         ,it("finalizes a specification expression function result", check_specification_expression) &
         ,it("finalizes an intent(out) derived type dummy argument", check_intent_out_finalization) &
         ,it("finalizes an allocatable component object", check_allocatable_component_finalization) &
      ])
  end function

  function construct_object() result(object)
    !! Constructor for object_t
    type(object_t) object
    object % dummy = avoid_unused_variable_warning
  end function

  subroutine count_finalizations(self)
    !! Destructor for object_t
    type(object_t), intent(inout) :: self
    finalizations = finalizations + 1
    self % dummy = avoid_unused_variable_warning
  end subroutine

  function check_lhs_object() result(result_)
    !! Verify Fortran 2018 clause 7.5.6.3, paragraph 1 behavior: "not an unallocated allocatable variable"
    type(object_t) lhs, rhs
    type(result_t) result_
    integer initial_tally

    rhs%dummy = avoid_unused_variable_warning
    initial_tally = finalizations
    lhs = rhs ! finalizes lhs
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate
  end function

  function check_allocated_allocatable_lhs() result(result_)
    !! Verify Fortran 2018 clause 7.5.6.3, paragraph 1 behavior: "allocated allocatable variable"
    type(object_t), allocatable :: lhs
    type(object_t) rhs
    type(result_t) result_
    integer initial_tally

    rhs%dummy = avoid_unused_variable_warning
    initial_tally = finalizations
    allocate(lhs)
    lhs = rhs ! finalizes lhs
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate
  end function

  function check_target_deallocation() result(result_)
    !! Verify Fortran 2018 clause 7.5.6.3, paragraph 2 behavior: "pointer is deallocated"
    type(object_t), pointer :: object_ptr => null()
    type(result_t) result_
    integer initial_tally

    allocate(object_ptr, source=object_t(dummy=0))
    initial_tally = finalizations
    deallocate(object_ptr) ! finalizes object
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate
  end function

  function check_allocatable_component_finalization() result(result_)
    !! Tests 7.5.6.3, para. 2 ("allocatable entity is deallocated") 
    !! + 9.7.3.2, para. 6 ("INTENT(OUT) allocatable dummy argument is deallocated")
    type(wrapper_t), allocatable :: wrapper
    type(result_t) result_
    integer initial_tally

    initial_tally = finalizations

    allocate(wrapper)
    allocate(wrapper%object)
    call finalize_intent_out_component(wrapper)
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate

  contains

    subroutine finalize_intent_out_component(output)
      type(wrapper_t), intent(out) :: output ! finalizes object component
      allocate(output%object)
      output%object%dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_finalize_on_deallocate() result(result_)
    !! Tests 7.5.6.3, paragraph 2: "allocatable entity is deallocated"
    type(object_t), allocatable :: object
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

  function check_finalize_on_end() result(result_)
    !! Tests 7.5.6.3, paragraph 3: "before return or END statement"
    type(result_t) result_
    integer initial_tally

    initial_tally = finalizations
    call finalize_on_end_subroutine() ! Finalizes local_obj
    associate(final_tally => finalizations - initial_tally)
      result_ = assert_equals(1, final_tally)
    end associate

  contains

    subroutine finalize_on_end_subroutine()
      type(object_t) local_obj
      local_obj % dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_block_finalization() result(result_)
    !! Tests 7.5.6.3, paragraph 4: "termination of the BLOCK construct"
    type(result_t) result_
    integer initial_tally

    initial_tally = finalizations
    block
      type(object_t) object
      object % dummy = avoid_unused_variable_warning
    end block ! Finalizes object
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate
  end function

  function check_rhs_function_reference() result(result_)
    !! Verify Fortran 2018 clause 7.5.6.3, paragraph 5 behavior: "nonpointer function result"
    type(object_t), allocatable :: object
    type(result_t) result_
    integer initial_tally

    initial_tally = finalizations
    object = construct_object() ! finalizes object_t result
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate
  end function

  function check_specification_expression() result(result_)
    !! Tests 7.5.6.3, paragraph 6: "specification expression function result"
    type(result_t) result_
    integer initial_tally

    initial_tally = finalizations
    call finalize_specification_expression
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate

  contains

    subroutine finalize_specification_expression
      type(object_t) :: object = object_t(dummy=0) ! Finalizes RHS function reference
      object%dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_intent_out_finalization() result(result_)
    !! Tests 7.5.6.3, paragraph 7: "nonpointer, nonallocatable, INTENT (OUT) dummy argument"
    type(result_t) result_
    type(object_t) object
    integer initial_tally

    initial_tally = finalizations
    call finalize_intent_out_arg(object)
    associate(delta => finalizations - initial_tally)
      result_ = assert_equals(1, delta)
    end associate
  contains
    subroutine finalize_intent_out_arg(output)
      type(object_t), intent(out) :: output ! finalizes output
      output%dummy = avoid_unused_variable_warning
    end subroutine
  end function

end module compiler_test
