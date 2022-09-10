module shallow_m
    use reference_counter_m, only: ref_reference_t

    implicit none
    private
    public :: shallow_t, resource_freed

    type, extends(ref_reference_t) :: shallow_t
        integer, pointer :: ref => null()
    contains
        procedure :: free
    end type

    interface shallow_t
        module procedure construct
    end interface

    integer, allocatable, target, save :: resource
    logical, save :: resource_freed = .false.

contains
    function construct() result(shallow)
        type(shallow_t) :: shallow

        resource = 42
        shallow%ref => resource
        call shallow%start_ref_counter
    end function

    subroutine free(self)
        class(shallow_t), intent(inout) :: self

        deallocate(resource)
        nullify(self%ref)
        resource_freed = .true.
    end subroutine

end module
