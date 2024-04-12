module smart_pointer_m
  implicit none

  type, abstract :: resource_t
  contains
      procedure(free_interface), deferred :: free
  end type

  abstract interface
    subroutine free_interface(self)
      import resource_t
      class(resource_t), intent(inout) :: self
    end subroutine
  end interface

  type reference_counter_t
    integer, pointer :: count_ => null()
    class(resource_t), pointer :: object_ => null()
  contains
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign_reference_counter
    generic :: assignment(=) => assign_reference_counter
    final :: finalize
  end type

  type, abstract, extends(resource_t) :: smart_pointer_t
    type(reference_counter_t) :: counter
  contains
    procedure, non_overridable :: release_handle
    procedure, non_overridable :: start_counter
  end type

contains

  subroutine finalize(self)
    type(reference_counter_t), intent(inout) :: self
    print *,"        reference_counter_t%finalize: start"
    if (associated(self%count_)) call self%release
    print *,"        reference_counter_t%finalize: end" // new_line('')
  end subroutine

  function construct_reference_counter_t(object) result(reference_counter)
    class(resource_t), intent(in) :: object
    type(reference_counter_t) reference_counter
    print *,"        construct_reference_counter_t: start"
    allocate(reference_counter%count_, source=0)
    allocate(reference_counter%object_, source=object)
    call reference_counter%grab
    print *,"        construct_reference_counter_t: end" // new_line('')
  end function

  subroutine grab(self)
    class(reference_counter_t), intent(inout) :: self
    print *,"          reference_counter_t%grab: start"
    if (.not. associated(self%count_)) error stop "reference_counter_t%grab: associated(self%count_)"
    self%count_ = self%count_ + 1
    print *,"          reference_counter_t%grab: end (self%count_ = ", self%count_,")"
  end subroutine

  subroutine release(self)
    class (reference_counter_t), intent(inout) :: self
    print *,"          reference_counter_t%release: start"
    if (.not. associated(self%count_)) error stop "reference_counter_t%grab: associated(self%count_)"
    self%count_ = self%count_ - 1
    if (self%count_ == 0) then ! <------- changing 0 to -1 eliminates the reference to undefined data
      print *,"          reference_counter_t%release: self%count_ = ", self%count_
      call self%object_%free
      deallocate (self%count_, self%object_)
    else
      print *,"          reference_counter_t%release: self%count_=",self%count_,", set reference_counter_t%{count_,object_}=>null()"
      self%count_ => null()
      self%object_ => null()
    end if
    print *,"          reference_counter_t%release: end"
  end subroutine

  subroutine assign_reference_counter(lhs, rhs)
    class(reference_counter_t), intent(inout) :: lhs
    class(reference_counter_t), intent(in) :: rhs
    print *,"        reference_counter_t%assign_reference_counter: start"
    if (.not. associated(rhs%count_)) error stop "reference_counter_t%assign_reference_counter: associated(rhs%count_)"
    lhs%count_ => rhs%count_
    lhs%object_ => rhs%object_
    call lhs%grab
    print *,"        reference_counter_t%assign_reference_counter: end" // new_line('')
  end subroutine

  subroutine release_handle(self)
    class(smart_pointer_t), intent(inout) :: self
    print *,"  smart_pointer_t%release_handle: start"
    call self%counter%release
    print *,"  smart_pointer_t%release_handle: end" // new_line('')
  end subroutine

  subroutine start_counter(self)
    class(smart_pointer_t), intent(inout) :: self
    print *,"      smart_pointer_t%start_counter: start" // new_line('')
    self%counter = construct_reference_counter_t(self)
    print *,"      smart_pointer_t%start_counter: end" // new_line('')
  end subroutine

end module

module integer_pointer_m 
  use smart_pointer_m, only: smart_pointer_t
  implicit none

  type, extends(smart_pointer_t) :: integer_pointer_t
    integer, pointer :: ref => null()
  contains
    procedure :: free
  end type

  integer, allocatable, target :: allocatable_integer
  integer, parameter :: the_answer = 42

contains

  function allocate_integer() result(integer_pointer)
    type(integer_pointer_t) integer_pointer
    print *,"    allocate_integer: start" // new_line('')
    if (.not. allocated(allocatable_integer)) allocate(allocatable_integer, source=the_answer)
    integer_pointer%ref => allocatable_integer
    integer_pointer%ref = the_answer
    call integer_pointer%start_counter
    print *,"    allocate_integer: end" //new_line('')
  end function

  subroutine free(self)
    class(integer_pointer_t), intent(inout) :: self
    print *,"          integer_pointer_t%free: start"
    if (allocated(allocatable_integer)) deallocate(allocatable_integer)
    nullify(self%ref)
    print *,"          integer_pointer_t%free: end" // new_line('')
  end subroutine

end module

program main
  use integer_pointer_m, only : integer_pointer_t, allocate_integer
  implicit none

  print *,"main: start" // new_line('')
  call test_reference_counting
  print *,"main: end" ! ---> this line is not reached <---

contains

  subroutine test_reference_counting
    type(integer_pointer_t) integer_pointer
    print *,"  main(check_creation): start" // new_line('')
    integer_pointer = allocate_integer()
    print *,"  main(check_creation): end" // new_line('')
  end subroutine

end program
