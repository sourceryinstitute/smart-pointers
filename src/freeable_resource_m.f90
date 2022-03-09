module freeable_resource_m
  implicit none

  private
  public :: freeable_resource_t

  type, abstract :: freeable_resource_t
  contains
      procedure(free_memory), deferred :: cpp_delete
  end type

  abstract interface

     subroutine free_memory (this)
        import :: freeable_resource_t
        class(freeable_resource_t), intent(inout) :: this
     end subroutine

  end interface

end module
