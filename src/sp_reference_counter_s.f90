submodule(sp_reference_counter_m) sp_reference_counter_s
  use assert_m, only : assert
  implicit none

contains

  module procedure reference_count
    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")
    counter = self%count_
  end procedure

  module procedure construct
    allocate(sp_reference_counter%count_, source=0)
    allocate(sp_reference_counter%object_, source=object)
    call sp_reference_counter%grab
  end procedure

  module procedure grab
    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")
    self%count_ = self%count_ + 1
  end procedure

  module procedure release

    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")

    self%count_ = self%count_ - 1

    if (self%count_ == 0) then
      call self%object_%free
      deallocate (self%count_, self%object_)
    else
      self%count_ => null()
      self%object_ => null()
    end if
  end procedure

  module procedure assign_sp_reference_counter
    lhs%count_ => rhs%count_
    lhs%object_ => rhs%object_
    call lhs%grab
  end procedure

end submodule sp_reference_counter_s
