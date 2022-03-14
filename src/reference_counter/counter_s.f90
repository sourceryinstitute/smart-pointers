submodule(counter_m) counter_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    allocate(counter%count_, source=0)
    allocate(counter%object_, source=object)
    call counter%grab
  end procedure

  module procedure grab
    call assert(associated(self%count_),"counter_t%grab: associated(self%count_)")
    self%count_ = self%count_ + 1
  end procedure

  module procedure release

    call assert(associated(self%count_),"counter_t%grab: associated(self%count_)")

    self%count_ = self%count_ - 1

    if (self%count_ == 0) then
      call self%object_%free_resource
      deallocate (self%count_, self%object_)
    else
      self%count_ => null()
      self%object_ => null()
    end if
  end procedure

  module procedure assign_counter
    lhs%count_ => rhs%count_
    lhs%object_ => rhs%object_
    call lhs%grab
  end procedure

  module procedure finalize
    if (associated(self%count_)) call self%release
  end procedure

end submodule counter_s
