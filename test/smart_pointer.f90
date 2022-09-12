module foo_m
  implicit none

  private
  public :: foo_t

  type foo_t
  end type

end module

module smart_pointer_m
  use reference_counter_m, only: ref_reference_t
  use foo_m, only : foo_t

  implicit none
  private
  public :: smart_pointer_t

  type, extends(ref_reference_t) :: smart_pointer_t
    type(foo_t), pointer :: ref => null()
  contains
    procedure :: free
  end type

  interface smart_pointer_t

    module function construct(foo) result(smart_pointer)
      implicit none
      type(foo_t), intent(in), target :: foo
      type(smart_pointer_t) :: smart_pointer
    end function

  end interface

  interface

    module subroutine free(self)
      implicit none
      class(smart_pointer_t), intent(inout) :: self
    end subroutine

  end interface

end module

submodule(smart_pointer_m) smart_pointer_s
  implicit none

contains

  module procedure construct
    smart_pointer%ref => foo
    call smart_pointer%start_ref_counter
  end procedure

  module procedure free
    if (associated(self%ref)) then
      deallocate(self%ref)
      nullify(self%ref)
      print *,"free(): foo deallocated"
    end if
  end procedure

end submodule

program main
  use smart_pointer_m, only : smart_pointer_t
  use foo_m, only : foo_t
  implicit none

  block 

    type(smart_pointer_t) ptr_1, ptr_2
    type(foo_t), pointer :: foo => null()

    allocate(foo, source = foo_t())
    ptr_1 = smart_pointer_t(foo)  ! 1st reference
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
