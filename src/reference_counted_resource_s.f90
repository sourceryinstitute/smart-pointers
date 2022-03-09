submodule(reference_counted_resource_m) reference_counted_resource_s
  implicit none

contains

  module subroutine release_handle(this)
    class(reference_counted_resource_t), intent(inout) :: this
    call this%counter%release
  end subroutine

  module subroutine start_counter(this)
    class(reference_counted_resource_t), intent(inout) :: this
    this%counter = reference_counter_t(this)
  end subroutine

end submodule
