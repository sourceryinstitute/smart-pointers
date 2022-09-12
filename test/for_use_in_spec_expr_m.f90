module for_use_in_spec_expr_m
    !! This module supports the specification expression finalization
    !! test in compiler_test.f90, check_specification_expression
    implicit none

    private
    public :: finalizable_t, component, was_finalized

    type finalizable_t
        private
        integer, pointer :: component_ => null()
    contains
        final :: finalize
    end Type

    interface finalizable_t
        module procedure construct
    end interface

    logical :: was_finalized = .false.

    interface
        pure subroutine lie()
        end subroutine
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
        call lie()
    end subroutine

end module

subroutine lie()
    use for_use_in_spec_expr_m, only: was_finalized
    was_finalized = .true.
end subroutine