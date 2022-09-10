module finalizable_m
  !! This module supports the specification_expression_finalization main program 
  !! (at the bottom of this file), which in turn supports the check_specification_expression 
  !! unit-test function in ../test/compiler_test.f90.
  implicit none

  private
  public :: finalizable_t, component

  type finalizable_t
    private
    integer, pointer :: component_ => null()
  contains
    final :: finalize
  end Type

  interface finalizable_t
    module procedure construct
  end interface

contains

  pure function construct(component) result(finalizable)
    integer, intent(in) :: component
    type(finalizable_t) finalizable
    allocate(finalizable%component_, source = component)
  end function

  pure function component(self) result(self_component)
    type(finalizable_t), intent(in) :: self
    integer self_component
    if (.not. associated(self%component_)) error stop "component: unassociated component"
    self_component = self%component_
  end function

  pure subroutine finalize(self)
    type(finalizable_t), intent(inout) :: self
    if (associated(self%component_)) deallocate(self%component_)
    error stop "finalize: intentional error termination to verify finalization"
  end subroutine

end module

program specification_expression_finalization
  !! Test the finalization of a function result in a specification expression
  use finalizable_m, only : finalizable_t, component
  implicit none

  call finalize_specification_expression_result

contains

  subroutine finalize_specification_expression_result
    real tmp(component(finalizable_t(component=0))) !! Finalizes the finalizable_t function result
  end subroutine

end program
