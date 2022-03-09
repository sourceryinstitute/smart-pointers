submodule(reference_counter_m) reference_counter_s
  use assert_m, only : assert
  implicit none

contains

  module procedure construct
    allocate(reference_counter%count_, source=0)
    allocate(reference_counter%object_, source=object)
    call reference_counter%grab
  end procedure

  module procedure grab
    call assert(associated(this%count_),"reference_counter_t%grab: associated(this%count_)")
    this%count_ = this%count_ + 1
  end procedure

  module procedure release

    call assert(associated(this%count_),"reference_counter_t%grab: associated(this%count_)")

    this%count_ = this%count_ - 1

    if (this%count_ == 0) then
      call this%object_%free_resource
      deallocate (this%count_, this%object_)
    else
      this%count_ => null()
      this%object_ => null()
    end if
  end procedure

  module procedure assign_reference_counter
    lhs%count_ => rhs%count_
    lhs%object_ => rhs%object_
    call lhs%grab
  end procedure

  module procedure finalize
    if (associated(this%count_)) call this%release
  end procedure

end submodule reference_counter_s
