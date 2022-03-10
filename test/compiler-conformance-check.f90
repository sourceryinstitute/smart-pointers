module inner_object_m
  implicit none
  private
  public :: inner_object_t, finalizations

  type inner_object_t
    private
    integer, allocatable :: i
  contains
    final :: finalize
  end type

  interface inner_object_t
    module procedure  construct
  end interface 

  integer :: finalizations = 0

contains
  
  function construct(item) result(inner_object)
    type(inner_object_t)  :: inner_object
    integer, intent(in) :: item
    inner_object%i = item !! allocation on assignment
  end function

  subroutine finalize(self)
    type(inner_object_t), intent(inout) :: self
    finalizations = finalizations + 1 
    if (allocated(self%i)) deallocate(self%i)
  end subroutine

end module

module outer_object_m
  use inner_object_m, only: inner_object_t
  implicit none
  private
  public :: outer_object_t, define

  type outer_object_t
    private
    type(inner_object_t), allocatable  :: inner_object
  end type 

contains

  subroutine define(outer_object, item)
    type(outer_object_t), intent(out) :: outer_object ! finalizes inner_object component
    integer item
    allocate(outer_object%inner_object)
    outer_object%inner_object = inner_object_t(item) ! finalizes inner_object_t result
  end subroutine

end module

program check_compiler_conformance
  use outer_object_m, only : outer_object_t, define
  use inner_object_m, only : inner_object_t, finalizations
  use assert_m, only : assert
  implicit none

  type(outer_object_t), allocatable  :: outer_object

  allocate(outer_object)
  call define(outer_object, item=10)
  deallocate(outer_object) ! finalizes inner_object component

  call assert(finalizations==3, "finalizations==3")
end program
