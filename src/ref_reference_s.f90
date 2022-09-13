submodule(ref_reference_m) ref_reference_s
  implicit none

contains

  module procedure reference_count
    counter = self%ref_counter%reference_count()
  end procedure

  module procedure release_handle
    call self%ref_counter%release
  end procedure

  module procedure start_ref_counter
    self%ref_counter = ref_counter_t(self)
  end procedure

end submodule
