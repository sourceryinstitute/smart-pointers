module reference_counter_m
  use freeable_resource_m, only : freeable_resource_t
  implicit none

  private
  public :: reference_counter_t

  type reference_counter_t
    private
    integer, pointer :: count_ => null()
    class(freeable_resource_t), pointer :: object_ => null()
  contains
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign_reference_counter
    generic :: assignment(=) => assign_reference_counter
    final :: finalize
  end type

  interface reference_counter_t

    module function construct(object) result(reference_counter)
      implicit none
      class(freeable_resource_t), intent(in) :: object
      type(reference_counter_t) reference_counter
     end function

  end interface

  interface

    module subroutine grab(self)
      implicit none
      class(reference_counter_t), intent(inout) :: self
    end subroutine

    module subroutine release(self)
      implicit none
      class (reference_counter_t), intent(inout) :: self
    end subroutine

    module subroutine assign_reference_counter(lhs, rhs)
      implicit none
      class(reference_counter_t), intent(inout) :: lhs
      class(reference_counter_t), intent(in) :: rhs
    end subroutine

    module subroutine finalize(self)
      implicit none
      type(reference_counter_t), intent(inout) :: self
    end subroutine

  end interface

end module reference_counter_m
