module freeable_resource_m
  implicit none

  private
  public :: freeable_resource_t

  type, abstract :: freeable_resource_t
  contains
      procedure(free_interface), deferred :: free_resource
  end type

  abstract interface

    subroutine free_interface(self)
      import freeable_resource_t
      class(freeable_resource_t), intent(inout) :: self
    end subroutine

  end interface

end module
