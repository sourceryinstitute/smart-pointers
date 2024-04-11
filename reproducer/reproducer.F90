module assert_m
  implicit none
contains
  pure subroutine assert(assertion, description)
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) error stop description
  end subroutine
end module

module sp_resource_m
  implicit none
  type, abstract :: sp_resource_t
  contains
      procedure(free_interface), deferred :: free
  end type
  abstract interface
    impure elemental subroutine free_interface(self)
      import sp_resource_t
      class(sp_resource_t), intent(inout) :: self
    end subroutine
  end interface
end module

module sp_reference_counter_m
  use sp_resource_m, only : sp_resource_t
  use assert_m, only : assert
  implicit none

  type sp_reference_counter_t
    integer, pointer :: count_ => null()
    class(sp_resource_t), pointer :: object_ => null()
  contains
    procedure :: reference_count
    procedure, non_overridable :: grab
    procedure, non_overridable :: release
    procedure :: assign_sp_reference_counter
    generic :: assignment(=) => assign_sp_reference_counter
    final :: finalize
  end type

  interface sp_reference_counter_t
    module procedure construct
  end interface

contains

  subroutine finalize(self)
    type(sp_reference_counter_t), intent(inout) :: self
    if (associated(self%count_)) call self%release
  end subroutine

  pure function reference_count(self) result(counter)
    class(sp_reference_counter_t), intent(in) :: self
    integer counter

    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")
    counter = self%count_
  end function

  function construct(object) result(sp_reference_counter)
    class(sp_resource_t), intent(in) :: object
    type(sp_reference_counter_t) sp_reference_counter
    print *,"sp_reference_counter_s(construct): allocate(sp_reference_counter%count_, source=0)"
    allocate(sp_reference_counter%count_, source=0)
    allocate(sp_reference_counter%object_, source=object)
    call sp_reference_counter%grab
  end function

  subroutine grab(self)
    class(sp_reference_counter_t), intent(inout) :: self
    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")
    print *,"sp_reference_counter_s(grab): self%count_ = self%count_ + 1"
    self%count_ = self%count_ + 1
    print *,"sp_reference_counter_s(grab): self%count_ = ", self%count_
  end subroutine

  subroutine release(self)
    class (sp_reference_counter_t), intent(inout) :: self
    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")
    print *,"sp_reference_counter_s(release): self%count_ = self%count_ - 1"
    self%count_ = self%count_ - 1
    if (self%count_ == 0) then
      call self%object_%free
      deallocate (self%count_, self%object_)
    else
      self%count_ => null()
      self%object_ => null()
    end if
  end subroutine

  subroutine assign_sp_reference_counter(lhs, rhs)
    class(sp_reference_counter_t), intent(inout) :: lhs
    class(sp_reference_counter_t), intent(in) :: rhs
    call assert(associated(rhs%count_),"sp_reference_counter_s(assign_sp_reference_counter): associated(self%count_)")
    print *,"sp_reference_counter_s(assign_sp_reference_counter): lhs%count_ => rhs%count_"
    lhs%count_ => rhs%count_
    lhs%object_ => rhs%object_
    call lhs%grab
  end subroutine

end module

module sp_smart_pointer_m
  use sp_resource_m, only: sp_resource_t
  use sp_reference_counter_m, only: sp_reference_counter_t
  implicit none

  type, abstract, extends(sp_resource_t) :: sp_smart_pointer_t
    private
    type(sp_reference_counter_t) :: counter
  contains
    procedure :: reference_count
    procedure, non_overridable :: release_handle
    procedure, non_overridable :: start_counter
  end type

  interface
    pure module function reference_count(self) result(counter)
      implicit none
      class(sp_smart_pointer_t), intent(in) :: self
      integer counter
    end function

    module subroutine release_handle(self)
      implicit none
      class(sp_smart_pointer_t), intent(inout) :: self
    end subroutine

    module subroutine start_counter(self)
      implicit none
      class(sp_smart_pointer_t), intent(inout) :: self
    end subroutine
  end interface
contains
  module procedure reference_count
    counter = self%counter%reference_count()
  end procedure

  module procedure release_handle
    print *,"sp_smart_pointer_s(release_handle): call self%counter%release"
    call self%counter%release
  end procedure

  module procedure start_counter
    self%counter = sp_reference_counter_t(self)
  end procedure
end module

module sp_smart_pointer_test_m
  use sp_smart_pointer_m, only: sp_smart_pointer_t
  implicit none

  type, extends(sp_smart_pointer_t) :: object_t
    integer, pointer :: ref => null()
  contains
    procedure :: free
  end type

  interface object_t
    module procedure construct
  end interface

  integer, allocatable, target :: the_resource
  integer, parameter :: the_answer = 42
contains
  function construct() result(object)
    type(object_t) :: object
    if (.not. allocated(the_resource)) allocate(the_resource, source=the_answer)
    object%ref => the_resource
    object%ref = the_answer
    call object%start_counter
  end function

  impure elemental subroutine free(self)
    class(object_t), intent(inout) :: self
    if (allocated(the_resource)) deallocate(the_resource)
    nullify(self%ref)
  end subroutine
end module sp_smart_pointer_test_m

  use sp_smart_pointer_test_m
  implicit none
  call check_creation
contains
  subroutine check_creation
    type(object_t) :: object
    object = object_t()
  end subroutine
end
