module sp_smart_pointer_m
  use sp_resource_m, only: sp_resource_t
  use sp_reference_counter_m, only: sp_reference_counter_t
  implicit none

  private
  public :: sp_smart_pointer_t

  type, abstract, extends(sp_resource_t) :: sp_smart_pointer_t
    type(sp_reference_counter_t) :: counter
  contains
    procedure :: reference_count
    procedure, non_overridable :: release_handle
    procedure, non_overridable :: start_counter
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

    module subroutine start_counter(self)
      implicit none
      class(sp_smart_pointer_t), intent(inout) :: self
    end subroutine

  end interface

end module sp_smart_pointer_m
