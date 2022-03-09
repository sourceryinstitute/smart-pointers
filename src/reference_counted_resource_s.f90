submodule(reference_counted_resource_m) reference_counted_resource_s
  implicit none

contains

  module procedure release_handle
    call this%counter%release
  end procedure

  module procedure start_counter
    this%counter = reference_counter_t(this)
  end procedure

end submodule
