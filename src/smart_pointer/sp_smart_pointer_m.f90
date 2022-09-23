module sp_smart_pointer_m
  use ref_resource_m, only: ref_resource_t
  use ref_counter_m, only: ref_counter_t
  implicit none

  private
  public :: sp_smart_pointer_t

  type, abstract, extends(ref_resource_t) :: sp_smart_pointer_t
    type(ref_counter_t) :: ref_counter
  contains
    procedure :: reference_count
    procedure, non_overridable :: release_handle
    procedure, non_overridable :: start_ref_counter
  end type

  interface

    pure module function reference_count(self) result(counter)
      implicit none
      class(sp_smart_pointer_t), intent(in) :: self
      integer counter
    end function

    module subroutine release_handle(self)
      implicit none
      class(sp_smart_pointer_t), intent(inout) :: self
    end subroutine

    module subroutine start_ref_counter(self)
      implicit none
      class(sp_smart_pointer_t), intent(inout) :: self
    end subroutine

  end interface

end module sp_smart_pointer_m
