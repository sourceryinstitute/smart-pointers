submodule(reference_counter_m) reference_counter_s
  implicit none

contains

  module function construct(object) result(reference_counter)
    class(freeable_resource_t), intent(in) :: object
    type(reference_counter_t) reference_counter
    !type(ref_counter), allocatable :: new_ref_counter
    !allocate (new_ref_counter)
    allocate (reference_counter%count_, source=0)
    allocate (reference_counter%object_, source=object)
    call reference_counter%grab
  end function

  module subroutine grab(this)
    class(reference_counter_t), intent(inout) :: this
    if (associated(this%count_)) then
      this%count_ = this%count_ + 1
    else; stop 'Error in grab: count not associated'
    end if
  end subroutine

  module subroutine release(this)
    class (reference_counter_t), intent(inout) :: this
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
  end subroutine

  module subroutine assign_reference_counter(lhs, rhs)
    class (reference_counter_t), intent(inout) :: lhs
    class (reference_counter_t), intent(in) :: rhs
    print*, 'assign in ref_counter'
    lhs%count_ => rhs%count_; lhs%object_ => rhs%object_
    call lhs%grab
  end subroutine

  recursive module subroutine finalize(this)
     type(reference_counter_t), intent(inout) :: this
     print*,'called FINAL'
    if (associated(this%count_)) call this%release
  end subroutine

end submodule reference_counter_s
