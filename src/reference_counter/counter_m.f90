module counter_m
  use freeable_resource_m, only : freeable_resource_t
  implicit none

  private
  public :: counter_t

  type counter_t
    private
    integer, pointer :: count_ => null()
    class(freeable_resource_t), pointer :: object_ => null()
  contains
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign_counter
    generic :: assignment(=) => assign_counter
    final :: finalize
  end type

  interface counter_t

    module function construct(object) result(counter)
      implicit none
      class(freeable_resource_t), intent(in) :: object
      type(counter_t) counter
     end function

  end interface

  interface

    module subroutine grab(self)
      implicit none
      class(counter_t), intent(inout) :: self
    end subroutine

    module subroutine release(self)
      implicit none
      class (counter_t), intent(inout) :: self
    end subroutine

    module subroutine assign_counter(lhs, rhs)
      implicit none
      class(counter_t), intent(inout) :: lhs
      class(counter_t), intent(in) :: rhs
    end subroutine

    module subroutine finalize(self)
      implicit none
      type(counter_t), intent(inout) :: self
    end subroutine

  end interface

end module counter_m
