module universal_interface
  use hermetic_interface ,only: hermetic
  use ref_counter_implementation ,only: ref_counter
  implicit none
  private
  public :: universal
  type ,abstract ,extends(hermetic) :: universal
    type(ref_counter) :: counter
  contains
    procedure, non_overridable :: force_finalize
    procedure, non_overridable :: register_self
  end type
contains
  subroutine force_finalize (this)
    class(universal), intent(inout) :: this
    call this%counter%release
  end subroutine
  subroutine register_self (this)
    class(universal), intent(inout) :: this
    this%counter = ref_counter(this)
  end subroutine
end module
