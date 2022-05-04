module compiler_test
  use vegetables, only: result_t, test_item_t, describe, it, assert_equals
  implicit none

  private
  public :: test_ref_reference

  type object_t
    private
    integer :: dummy
  contains
    final :: count_finalizations
  end type

  interface object_t
    module procedure construct_object
  end interface

  type wrapper_t
    private
    type(object_t), allocatable :: object
  end type

  type elem_t
    private
    integer :: dummy
  contains
    final :: count_elemental_finalizations
  end type

  interface elem_t
    module procedure construct_elem
  end interface elem_t

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
         ,it("finalizes a non-allocatable object on the RHS of an intrinsic assignment", check_rhs_object_assignment) &
         ,it("finalizes a function reference on the RHS of an intrinsic assignment", check_rhs_function_reference) &
         ,it("finalizes an allocatable component object", check_allocatable_component_finalization) &
         ,it("finalizes a non-pointer non-allocatable object at the end of a block construct", check_block_finalization) &
         ,it("finalizes a non-pointer non-allocatable array object at the END statement", check_finalize_on_end) &
      ])
  end function

  function construct_object() result(object)
    !! Constructor for object_t
    type(object_t) :: object
    object % dummy = avoid_unused_variable_warning
  end function

  subroutine count_finalizations(self)
    !! Destructor for object_t
    type(object_t), intent(inout) :: self
    print *, 'Finalizing object...'
    finalizations = finalizations + 1
    self % dummy = avoid_unused_variable_warning
  end subroutine

  elemental function construct_elem() result(elem)
    !! Constructor for elem_t
    type(elem_t) :: elem
    elem % dummy = avoid_unused_variable_warning
  end function

  elemental subroutine count_elemental_finalizations(self)
    !! Destructor for elem_t
    type(elem_t), intent(out) :: self
    print *, 'Finalizing element...'
    !$omp atomic update
    finalizations = finalizations + 1
    !$omp end atomic
    self % dummy = avoid_unused_variable_warning
  end subroutine

  function check_rhs_object_assignment() result(result_)
    !! Tests 7.5.6.3 case 1 (intrinsic assignment with non-allocatable variable)
    !! Expected: 1; gfortran 11.2: 0
    type(object_t) :: lhs, rhs
    type(result_t) :: result_
    integer :: initial_tally, delta

    rhs%dummy = avoid_unused_variable_warning
    initial_tally = finalizations
    lhs = rhs ! finalizes rhs
    delta = finalizations - initial_tally
    result_ = assert_equals(1, delta)
  end function

  function check_rhs_function_reference() result(result_)
    !! Tests 7.5.6.3 case 1 (intrinsic assignment with allocated variable)
    !! Expected: 1; gfortran 11.2: 0
    type(object_t), allocatable :: object
    type(result_t) :: result_
    integer :: initial_tally, delta

    initial_tally = finalizations
    object = object_t() ! finalizes object_t result
    delta = finalizations - initial_tally
    result_ = assert_equals(1, delta)
  end function

  function check_finalize_on_deallocate() result(result_)
    !! Tests 7.5.6.3 case 2 (explicit deallocation on allocatable entity)
    type(object_t), allocatable :: object
    type(result_t) :: result_
    integer :: initial_tally

    initial_tally = finalizations
    allocate(object)
    object%dummy = 1
    deallocate(object)          ! finalizes object
    associate(final_tally => finalizations - initial_tally)
      result_ = assert_equals(1, final_tally)
    end associate
  end function

  function check_finalize_on_end() result(result_)
    !! Tests 7.5.6.3 case 3
    type(result_t) :: result_
    integer :: initial_tally
    integer, parameter :: nelems = 2

    initial_tally = finalizations
    call finalize_on_end_subroutine()
    associate(final_tally => finalizations - initial_tally)
      result_ = assert_equals(nelems, final_tally)
    end associate

  contains

    subroutine finalize_on_end_subroutine()
      type(elem_t) :: array(nelems)
      array(:) % dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_block_finalization() result(result_)
    !! Tests 7.5.6.3 case 4
    type(result_t) :: result_
    integer :: initial_tally, delta

    initial_tally = finalizations
    block
      type(object_t) :: object
      object % dummy = avoid_unused_variable_warning
    end block ! Finalizes object
    delta = finalizations - initial_tally
    result_ = assert_equals(1, delta)
  end function

  function check_intent_out_finalization() result(result_)
    !! Tests 7.5.6.3 case 7 (non-pointer non-allocatable INTENT(OUT) dummy argument)
    type(result_t) :: result_
    type(object_t) :: object
    integer :: initial_tally

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
    !! Tests 7.5.6.3 cases 2 (allocatable entity) & 7
    type(wrapper_t), allocatable  :: wrapper
    type(result_t) :: result_
    integer :: initial_tally, delta

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
