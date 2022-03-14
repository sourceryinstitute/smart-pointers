module shadow_m
  use freeable_resource_m, only: freeable_resource_t
  use counter_m, only: counter_t
  implicit none

  private
  public :: shadow_t

  type, abstract, extends(freeable_resource_t) :: shadow_t
    type(counter_t) :: counter
  contains
    procedure, non_overridable :: release_handle
    procedure, non_overridable :: start_counter
  end type

  interface

    module subroutine release_handle(self)
      implicit none
      class(shadow_t), intent(inout) :: self
    end subroutine

    module subroutine start_counter(self)
      implicit none
      class(shadow_t), intent(inout) :: self
    end subroutine

  end interface

end module shadow_m
