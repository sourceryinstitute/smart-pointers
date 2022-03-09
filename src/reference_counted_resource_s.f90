submodule(reference_counted_resource_m) reference_counted_resource_s
  implicit none

contains

  module subroutine force_finalize (this)
    class(reference_counted_resource_t), intent(inout) :: this
    call this%counter%release
  end subroutine

  module subroutine register_self (this)
    class(reference_counted_resource_t), intent(inout) :: this
    this%counter = ref_counter(this)
  end subroutine

end submodule
