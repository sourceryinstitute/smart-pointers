module reference_counter_m
  use hermetic_interface, only : hermetic
  implicit none

  private
  public :: reference_counter_t

  type reference_counter_t
    private
    integer, pointer :: count => null()
    class(hermetic), pointer :: obj => null()
  contains
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign
    final :: finalize
    generic :: assignment(=) => assign
  end type

  interface reference_counter_t

    module function construct(object) result(reference_counter)
      implicit none
      class(hermetic), intent(in) :: object
      type(reference_counter_t) reference_counter
     end function

  end interface

  interface

    module subroutine grab(this)
      implicit none
      class(reference_counter_t), intent(inout) :: this
    end subroutine

    module subroutine release(this)
      implicit none
      class (reference_counter_t), intent(inout) :: this
    end subroutine

    module subroutine assign (lhs, rhs)
      implicit none
      class(reference_counter_t), intent(inout) :: lhs
      class(reference_counter_t), intent(in) :: rhs
    end subroutine

    recursive module subroutine finalize(this)
      implicit none
      type(reference_counter_t), intent(inout) :: this
    end subroutine

  end interface

end module reference_counter_m
