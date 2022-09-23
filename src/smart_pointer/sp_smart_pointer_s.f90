submodule(sp_smart_pointer_m) sp_smart_pointer_s
  implicit none

contains

  module procedure reference_count
    counter = self%counter%reference_count()
  end procedure

  module procedure release_handle
    call self%counter%release
  end procedure

  module procedure start_counter
    self%counter = sp_reference_counter_t(self)
  end procedure

end submodule
