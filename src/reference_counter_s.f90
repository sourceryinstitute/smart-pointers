submodule(reference_counter_m) reference_counter_s
  implicit none

contains

  module function construct(object) result(reference_counter)
    class(freeable_resource_t), intent(in) :: object
    type(reference_counter_t) reference_counter
    !type(ref_counter), allocatable :: new_ref_counter
    !allocate (new_ref_counter)
    allocate (reference_counter%count, source=0)
    allocate (reference_counter%obj, source=object)
    call reference_counter%grab
  end function

  module subroutine grab(this)
    class(reference_counter_t), intent(inout) :: this
    if (associated(this%count)) then
      this%count = this%count + 1
    else; stop 'Error in grab: count not associated'
    end if
  end subroutine

  module subroutine release(this)
    class (reference_counter_t), intent(inout) :: this
     print*,'release:  associated?',associated(this%count)
    if (associated(this%count)) then
       print*,'release:  count:',this%count
      this%count = this%count - 1
      if (this%count == 0) then
        call this%obj%cpp_delete; deallocate (this%count, this%obj)
      else; this%count => null(); this%obj => null()
      end if
    else; stop 'Error in release: count not associated'
    end if
  end subroutine

  module subroutine assign (lhs, rhs)
    class (reference_counter_t), intent(inout) :: lhs
    class (reference_counter_t), intent(in) :: rhs
    print*, 'assign in ref_counter'
    lhs%count => rhs%count; lhs%obj => rhs%obj
    call lhs%grab
  end subroutine

  recursive module subroutine finalize(this)
     type(reference_counter_t), intent(inout) :: this
     print*,'called FINAL'
    if (associated(this%count)) call this%release
  end subroutine

end submodule reference_counter_s
