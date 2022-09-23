module sp_reference_counter_m
  use sp_resource_m, only : sp_resource_t
  implicit none

  private
  public :: sp_reference_counter_t

  type sp_reference_counter_t
    private
    integer, pointer :: count_ => null()
    class(sp_resource_t), pointer :: object_ => null()
  contains
    procedure :: reference_count
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign_sp_reference_counter
    generic :: assignment(=) => assign_sp_reference_counter
    final :: finalize
  end type

  interface sp_reference_counter_t

    module function construct(object) result(sp_reference_counter)
      implicit none
      class(sp_resource_t), intent(in) :: object
      type(sp_reference_counter_t) sp_reference_counter
     end function

  end interface

  interface

    pure module function reference_count(self) result(counter)
      implicit none
      class(sp_reference_counter_t), intent(in) :: self
      integer counter
    end function

    module subroutine grab(self)
      implicit none
      class(sp_reference_counter_t), intent(inout) :: self
    end subroutine

    module subroutine release(self)
      implicit none
      class (sp_reference_counter_t), intent(inout) :: self
    end subroutine

    module subroutine assign_sp_reference_counter(lhs, rhs)
      implicit none
      class(sp_reference_counter_t), intent(inout) :: lhs
      class(sp_reference_counter_t), intent(in) :: rhs
    end subroutine

  end interface

contains

  subroutine finalize(self)
    type(sp_reference_counter_t), intent(inout) :: self
    if (associated(self%count_)) call self%release
  end subroutine

end module sp_reference_counter_m
