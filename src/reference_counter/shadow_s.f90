submodule(shadow_m) shadow_s
  implicit none

contains

  module procedure release_handle
    call self%counter%release
  end procedure

  module procedure start_counter
    self%counter = counter_t(self)
  end procedure

end submodule
