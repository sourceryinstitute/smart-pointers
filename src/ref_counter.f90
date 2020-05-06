module ref_counter_implementation
  use hermetic_interface ,only : hermetic
  private; public :: ref_counter
  type ref_counter
    private
    integer, pointer :: count => null()
    class(hermetic), pointer :: obj => null()
  contains
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign
    final :: finalize_ref_counter
    generic :: assignment(=) => assign
  end type
  interface ref_counter
    module procedure new_ref_counter; end interface
contains
  function new_ref_counter(object)
    class(hermetic), intent(in) :: object
    type(ref_counter), allocatable :: new_ref_counter
    allocate (new_ref_counter); allocate (new_ref_counter%count, source=0)
    allocate (new_ref_counter%obj, source=object)
    call new_ref_counter%grab; end function
  subroutine grab(this)
    class(ref_counter), intent(inout) :: this
    if (associated(this%count)) then
      this%count = this%count + 1
    else; stop 'Error in grab: count not associated'
    end if; end subroutine
  subroutine release(this)
    class (ref_counter), intent(inout) :: this
    if (associated(this%count)) then
      this%count = this%count - 1
      if (this%count == 0) then
        call this%obj%cpp_delete; deallocate (this%count, this%obj)
      else; this%count => null(); this%obj => null()
      end if
    else; stop 'Error in release: count not associated'
    end if; end subroutine
  subroutine assign (lhs, rhs)
    class (ref_counter), intent(inout) :: lhs
    class (ref_counter), intent(in) :: rhs
    lhs%count => rhs%count; lhs%obj => rhs%obj
    call lhs%grab; end subroutine
  recursive subroutine finalize_ref_counter (this)
    type(ref_counter), intent(inout) :: this
    if (associated(this%count)) call this%release; end subroutine
end module
