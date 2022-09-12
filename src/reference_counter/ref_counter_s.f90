submodule(ref_counter_m) ref_counter_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    allocate(ref_counter%count_, source=0)
    allocate(ref_counter%object_, source=object)
    call ref_counter%grab
  end procedure

  module procedure grab
    call assert(associated(self%count_),"ref_counter_t%grab: associated(self%count_)")
    self%count_ = self%count_ + 1
  end procedure

  module procedure release

    call assert(associated(self%count_),"ref_counter_t%grab: associated(self%count_)")

    self%count_ = self%count_ - 1

    if (self%count_ == 0) then
      call self%object_%free
      deallocate (self%count_, self%object_)
    else
      self%count_ => null()
      self%object_ => null()
    end if
  end procedure

  module procedure assign_ref_counter
    lhs%count_ => rhs%count_
    lhs%object_ => rhs%object_
    call lhs%grab
  end procedure

  module procedure counter
    reference_count = self%count_
  end procedure

end submodule ref_counter_s
