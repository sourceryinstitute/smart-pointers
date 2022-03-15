module ref_resource_m
  implicit none

  private
  public :: ref_resource_t

  type, abstract :: ref_resource_t
  contains
      procedure(free_interface), deferred :: free
  end type

  abstract interface

    subroutine free_interface(self)
      import ref_resource_t
      class(ref_resource_t), intent(inout) :: self
    end subroutine

  end interface

end module
