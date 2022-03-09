module reference_counted_resource_m
  use freeable_resource_m, only: freeable_resource_t
  use reference_counter_m, only: reference_counter_t
  implicit none

  private
  public :: reference_counted_resource_t

  type, abstract, extends(freeable_resource_t) :: reference_counted_resource_t
    type(reference_counter_t) :: counter
  contains
    procedure, non_overridable :: release_handle
    procedure, non_overridable :: start_counter
  end type

  interface

    module subroutine release_handle(this)
      implicit none
      class(reference_counted_resource_t), intent(inout) :: this
    end subroutine

    module subroutine start_counter(this)
      implicit none
      class(reference_counted_resource_t), intent(inout) :: this
    end subroutine

  end interface

end module reference_counted_resource_m