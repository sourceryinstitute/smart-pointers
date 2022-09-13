module ref_reference_m
  use ref_resource_m, only: ref_resource_t
  use ref_counter_m, only: ref_counter_t
  implicit none

  private
  public :: ref_reference_t

  type, abstract, extends(ref_resource_t) :: ref_reference_t
    type(ref_counter_t) :: ref_counter
  contains
    procedure :: reference_count
    procedure, non_overridable :: release_handle
    procedure, non_overridable :: start_ref_counter
  end type

  interface

    pure module function reference_count(self) result(counter)
      implicit none
      class(ref_reference_t), intent(in) :: self
      integer counter
    end function

    module subroutine release_handle(self)
      implicit none
      class(ref_reference_t), intent(inout) :: self
    end subroutine

    module subroutine start_ref_counter(self)
      implicit none
      class(ref_reference_t), intent(inout) :: self
    end subroutine

  end interface

end module ref_reference_m
