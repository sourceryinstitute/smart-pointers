submodule(reference_counted_resource_m) reference_counted_resource_s
  implicit none

contains

  module procedure release_handle
    call self%counter%release
  end procedure

  module procedure start_counter
    self%counter = reference_counter_t(self)
  end procedure

end submodule
