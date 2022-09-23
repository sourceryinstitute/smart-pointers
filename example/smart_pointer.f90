module user_object_m
  use reference_counter_m, only: ref_reference_t
  implicit none

  private
  public :: user_object_t, user_object_ptr_t

  type user_object_t
  end type

  type, extends(ref_reference_t) :: user_object_ptr_t
    type(user_object_t), pointer :: ref => null()
  contains
    procedure :: free
  end type

  interface user_object_ptr_t

    module function construct(user_object) result(user_object_ptr)
      implicit none
      type(user_object_t), intent(in), pointer:: user_object
      type(user_object_ptr_t) :: user_object_ptr
    end function

  end interface

  interface

    module subroutine free(self)
      implicit none
      class(user_object_ptr_t), intent(inout) :: self
    end subroutine

  end interface

end module

submodule(user_object_m) user_object_ptr_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    call assert(associated(user_object), "construct_from_pointer: associated(user_object)")
    user_object_ptr%ref => user_object
    call user_object_ptr%start_ref_counter
  end procedure

  module procedure free
    if (associated(self%ref)) then
      deallocate(self%ref)
      nullify(self%ref)
      print *,"free(): user_object deallocated"
    end if
  end procedure

end submodule

program main
  use user_object_m, only : user_object_t, user_object_ptr_t
  use assert_m, only : assert
  implicit none

  block 
    type(user_object_ptr_t) smart_pointer_1, smart_pointer_2
    type(user_object_t), pointer :: user_object => null()

    print *, "Allocating user_object pointer."
    allocate(user_object, source = user_object_t())

    print *, "Defining smart_pointer_1."
    smart_pointer_1 = user_object_ptr_t(user_object)
    print *, "Reference count = ", smart_pointer_1%reference_count()
    print *, "Copying smart_pointer_1 into smart_pointer_2."

    smart_pointer_2 = smart_pointer_1
    print *, "Reference count = ", smart_pointer_1%reference_count()
    call assert(smart_pointer_1%reference_count()==smart_pointer_2%reference_count(), "consistent counts")

    call new_reference(smart_pointer_2)
    call assert(smart_pointer_1%reference_count()==smart_pointer_2%reference_count(), "consistent counts")
    print *, "Reference count = ", smart_pointer_1%reference_count()
    print *, "smart_pointer_1 and smart_pointer_2 going out of scope"
  end block

contains

  subroutine new_reference(dummy_argument)
    type(user_object_ptr_t), intent(in) :: dummy_argument
    type(user_object_ptr_t) smart_pointer_3 

    print *, "Copying smart_pointer_2 into smart_pointer_3."
    smart_pointer_3 = dummy_argument
    call assert(dummy_argument%reference_count()==smart_pointer_3%reference_count(), "consistent counts")
    print *, "Reference count = ", smart_pointer_3%reference_count()
    print *, "smart_pointer_3 going out of scope."
  end subroutine

end program
