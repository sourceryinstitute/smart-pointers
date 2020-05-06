module hermetic_interface
  private
  public :: hermetic
  type, abstract :: hermetic
  contains
      procedure(free_memory), deferred :: cpp_delete
  end type
  abstract interface
     subroutine free_memory (this)
        import :: hermetic
        class(hermetic), intent(inout) :: this
     end subroutine
  end interface
end module
