submodule(reference_counter_m) reference_counter_s
  implicit none

contains

  module procedure construct
    !type(ref_counter), allocatable :: new_ref_counter
    !allocate (new_ref_counter)
    allocate (reference_counter%count_, source=0)
    allocate (reference_counter%object_, source=object)
    call reference_counter%grab
  end procedure

  module procedure grab
    if (associated(this%count_)) then
      this%count_ = this%count_ + 1
    else; stop 'Error in grab: count not associated'
    end if
  end procedure

  module procedure release
     print*,'release:  associated?',associated(this%count_)
    if (associated(this%count_)) then
       print*,'release:  count:',this%count_
      this%count_ = this%count_ - 1
      if (this%count_ == 0) then
        call this%object_%free_resource
        deallocate (this%count_, this%object_)
      else; this%count_ => null(); this%object_ => null()
      end if
    else; stop 'Error in release: count not associated'
    end if
  end procedure

  module procedure assign_reference_counter
    print*, 'assign in ref_counter'
    lhs%count_ => rhs%count_; lhs%object_ => rhs%object_
    call lhs%grab
  end procedure

  module procedure finalize
     print*,'called FINAL'
    if (associated(this%count_)) call this%release
  end procedure

end submodule reference_counter_s
