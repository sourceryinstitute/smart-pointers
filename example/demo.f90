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
    type(object_t), intent(in), target :: object

    smart_pointer%ref => object
    call smart_pointer%start_ref_counter
  end function

  subroutine free(self)
      class(smart_pointer_t), intent(inout) :: self

    if (associated(self%ref)) then
      deallocate(self%ref)
      nullify(self%ref)
      print *,"free(): object deallocated"
    end if
  end subroutine

end module

program main
  use smart_pointer_m, only : smart_pointer_t
  use object_m, only : object_t
  implicit none

  block 

    type(smart_pointer_t) ptr_1, ptr_2
    type(object_t), pointer :: object => null()

    allocate(object, source = object_t())
    ptr_1 = smart_pointer_t(object)  ! 1st reference
    print *, ptr_1%reference_count() 
    ptr_2 = ptr_1 ! 2nd reference
    print *, ptr_2%reference_count()
    call new_reference(ptr_2)
    print *, ptr_2%reference_count() ! 2 remaining references

  end block ! ref_reference_counter frees the memory after the 2 remaining references go out of scope

  print *,"All references gone"

contains

  subroutine new_reference(obj)
    type(smart_pointer_t), intent(in) :: obj
    type(smart_pointer_t) local_ptr

    local_ptr = obj                     ! 3rd reference
    print *, local_ptr%reference_count()
  end subroutine

end program
