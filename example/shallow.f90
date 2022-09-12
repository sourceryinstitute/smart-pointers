module object_m
  implicit none

  private
  public :: object_t

  type object_t
  end type

end module

module smart_pointer_m
    use reference_counter_m, only: ref_reference_t
    use object_m, only : object_t

    implicit none
    private
    public :: smart_pointer_t

    type, extends(ref_reference_t) :: smart_pointer_t
        type(object_t), pointer :: ref => null()
    contains
        procedure :: free
    end type

    interface smart_pointer_t
        module procedure construct
    end interface

contains

    function construct(object) result(smart_pointer)
        type(smart_pointer_t) :: smart_pointer
        type(object_t), intent(in) :: object

        allocate(smart_pointer%ref, source = object)
        call smart_pointer%start_ref_counter
    end function

    subroutine free(self)
        class(smart_pointer_t), intent(inout) :: self

        deallocate(self%ref)
        nullify(self%ref)
    end subroutine

end module

program main
  use smart_pointer_m, only : smart_pointer_t
  use object_m, only : object_t
  implicit none
   
  block 
    type(smart_pointer_t) smart_pointer, shallow_copy

    smart_pointer = smart_pointer_t(object_t())
    shallow_copy = smart_pointer  
  end block

end program
