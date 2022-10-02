module compiler_test_m
  !! Test compiler conformance with each scenario in which the Fortran 2018
  !! standard mandates type finalization.
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use iso_fortran_env, only : compiler_version
  implicit none

  private
  public :: compiler_test_t

  type, extends(test_t) :: compiler_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

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

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The compiler" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
       test_result_t("finalizes a non-allocatable object on the LHS of an intrinsic assignment", check_lhs_object()) &
      ,test_result_t("finalizes an allocated allocatable LHS of an intrinsic assignment", check_allocated_allocatable_lhs()) &
      ,test_result_t("finalizes a target when the associated pointer is deallocated", check_target_deallocation()) &
      ,test_result_t("finalizes an object upon explicit deallocation", check_finalize_on_deallocate()) &
      ,test_result_t("finalizes a non-pointer non-allocatable object at the END statement", check_finalize_on_end()) &
      ,test_result_t("finalizes a non-pointer non-allocatable object at the end of a block construct", check_block_finalization()) &
      ,test_result_t("finalizes a function reference on the RHS of an intrinsic assignment", check_rhs_function_reference()) &
      ,test_result_t("finalizes a specification expression function result", check_specification_expression()) &
      ,test_result_t("finalizes an intent(out) derived type dummy argument", check_intent_out_finalization()) &
      ,test_result_t("finalizes an allocatable component object", check_allocatable_component_finalization()) &
    ]
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

  function check_lhs_object() result(test_passes)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 1 behavior:
    !! "not an unallocated allocatable variable"
    type(object_t) lhs, rhs
    logical test_passes
    integer initial_tally

    rhs%dummy = avoid_unused_variable_warning
    initial_tally = finalizations
    lhs = rhs ! finalizes lhs
    associate(delta => finalizations - initial_tally)
      test_passes = delta == 1
    end associate
  end function

  function check_allocated_allocatable_lhs() result(test_passes)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 1 behavior:
    !! "allocated allocatable variable"
    type(object_t), allocatable :: lhs
    type(object_t) rhs
    logical test_passes
    integer initial_tally

    rhs%dummy = avoid_unused_variable_warning
    initial_tally = finalizations
    allocate(lhs)
    lhs = rhs ! finalizes lhs
    associate(delta => finalizations - initial_tally)
      test_passes = delta == 1
    end associate
  end function

  function check_target_deallocation() result(test_passes)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 2 behavior:
    !! "pointer is deallocated"
    type(object_t), pointer :: object_ptr
    logical test_passes
    integer initial_tally

    allocate(object_ptr, source=object_t(dummy=0))
    initial_tally = finalizations
    deallocate(object_ptr) ! finalizes object
    associate(delta => finalizations - initial_tally)
      test_passes = delta == 1 
    end associate
  end function

  function check_allocatable_component_finalization() result(test_passes)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, para. 2 ("allocatable entity is deallocated")
    !! + 9.7.3.2, para. 6 ("INTENT(OUT) allocatable dummy argument is deallocated")
    type(wrapper_t), allocatable :: wrapper
    logical test_passes
    integer initial_tally

    initial_tally = finalizations

    allocate(wrapper)
    allocate(wrapper%object)
    call finalize_intent_out_component(wrapper)
    associate(delta => finalizations - initial_tally)
      test_passes = delta == 1
    end associate

  contains

    subroutine finalize_intent_out_component(output)
      type(wrapper_t), intent(out) :: output ! finalizes object component
      allocate(output%object)
      output%object%dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_finalize_on_deallocate() result(test_passes)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 2:
    !! "allocatable entity is deallocated"
    type(object_t), allocatable :: object
    logical test_passes
    integer initial_tally

    initial_tally = finalizations
    allocate(object)
    object%dummy = 1
    deallocate(object)          ! finalizes object
    associate(final_tally => finalizations - initial_tally)
      test_passes = final_tally == 1
    end associate
  end function

  function check_finalize_on_end() result(test_passes)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 3:
    !! "before return or END statement"
    logical test_passes
    integer initial_tally

    initial_tally = finalizations
    call finalize_on_end_subroutine() ! Finalizes local_obj
    associate(final_tally => finalizations - initial_tally)
      test_passes = final_tally == 1
    end associate

  contains

    subroutine finalize_on_end_subroutine()
      type(object_t) local_obj
      local_obj % dummy = avoid_unused_variable_warning
    end subroutine

  end function

  function check_block_finalization() result(test_passes)
    !! Test conformance with Fortran 2018 clause  7.5.6.3, paragraph 4:
    !! "termination of the BLOCK construct"
    logical test_passes
    integer initial_tally

    initial_tally = finalizations
    block
      type(object_t) object
      object % dummy = avoid_unused_variable_warning
    end block ! Finalizes object
    associate(delta => finalizations - initial_tally)
      test_passes = delta == 1
    end associate
  end function

  function check_rhs_function_reference() result(test_passes)
    !! Test conformance with Fortran 2018 clause 7.5.6.3, paragraph 5 behavior:
    !! "nonpointer function result"
    type(object_t), allocatable :: object
    logical test_passes
    integer initial_tally

    initial_tally = finalizations
    object = construct_object() ! finalizes object_t result
    associate(delta => finalizations - initial_tally)
      test_passes = delta == 1
    end associate
  end function

  function check_specification_expression() result(test_passes)
    !! Test conformance with Fortran 2018 standard clause 7.5.6.3, paragraph 6:
    !! "specification expression function result"
    logical test_passes
    integer exit_status
    logical error_termination_occurred

    call execute_command_line( &
      command = "fpm run --example specification_expression_finalization "// fpm_compiler_arguments() //" > /dev/null 2>&1", &
      wait = .true., &
      exitstat = exit_status &
    )
    error_termination_occurred = exit_status /=0
    test_passes = error_termination_occurred

  contains

    function fpm_compiler_arguments() result(args)
      character(len=:), allocatable :: args

      associate(compiler_identity=>compiler_version())
        if (scan(compiler_identity, "GCC")==1) then
          args = " "
        else if (scan(compiler_identity, "NAG")==1) then
          args = "--compiler nagfor --flag -fpp"
        else if (scan(compiler_identity, "Intel")==1) then
          args = "--compiler ifort --flag -coarray=shared"
        else
          error stop "----> Unrecognized compiler_version() in function fpm_compiler_arguments. <----"
        end if
      end associate
    end function

  end function

  function check_intent_out_finalization() result(test_passes)
    !! Test conformance with Fortran 2018 standard clause 7.5.6.3, paragraph 7:
    !! "nonpointer, nonallocatable, INTENT (OUT) dummy argument"
    logical test_passes
    type(object_t) object
    integer initial_tally

    initial_tally = finalizations
    call finalize_intent_out_arg(object)
    associate(delta => finalizations - initial_tally)
      test_passes = delta == 1
    end associate
  contains
    subroutine finalize_intent_out_arg(output)
      type(object_t), intent(out) :: output ! finalizes output
      output%dummy = avoid_unused_variable_warning
    end subroutine
  end function

end module compiler_test_m
