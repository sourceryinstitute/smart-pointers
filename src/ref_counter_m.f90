module ref_counter_m
  use ref_resource_m, only : ref_resource_t
  implicit none

  private
  public :: ref_counter_t

  type ref_counter_t
    private
    integer, pointer :: count_ => null()
    class(ref_resource_t), pointer :: object_ => null()
  contains
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign_ref_counter
    generic :: assignment(=) => assign_ref_counter
    final :: finalize
  end type

  interface ref_counter_t

    module function construct(object) result(ref_counter)
      implicit none
      class(ref_resource_t), intent(in) :: object
      type(ref_counter_t) ref_counter
     end function

  end interface

  interface

    module subroutine grab(self)
      implicit none
      class(ref_counter_t), intent(inout) :: self
    end subroutine

    module subroutine release(self)
      implicit none
      class (ref_counter_t), intent(inout) :: self
    end subroutine

    module subroutine assign_ref_counter(lhs, rhs)
      implicit none
      class(ref_counter_t), intent(inout) :: lhs
      class(ref_counter_t), intent(in) :: rhs
    end subroutine

  end interface

contains

  subroutine finalize(self)
    type(ref_counter_t), intent(inout) :: self
    if (associated(self%count_)) call self%release
  end subroutine

end module ref_counter_m
