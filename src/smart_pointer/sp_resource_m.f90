module sp_resource_m
  implicit none

  private
  public :: sp_resource_t

  type, abstract :: sp_resource_t
  contains
      procedure(free_interface), deferred :: free
  end type

  abstract interface

    impure elemental subroutine free_interface(self)
      import sp_resource_t
      class(sp_resource_t), intent(inout) :: self
    end subroutine

  end interface

end module
