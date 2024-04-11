module assert_m
  !! Enforce logical assertions that can be toggled on/off at compile-time
  !! To turn off assertions, building with the flag -DUSE_ASSERTIONS=.false.
  implicit none

  private
  public :: assert




  logical, parameter :: enforce_assertions = .true.

  interface 

     pure module subroutine assert(assertion, description)
       !! Error terminate on .false. assertion with the stop code given by description
       !! With IBM XL Fortran, the stop code is an integer due to character stop codes being unsupported.
       implicit none
       logical, intent(in) :: assertion
       character(len=*), intent(in) :: description
     end subroutine

  end interface

end module assert_m
submodule(assert_m) assert_s
  implicit none

contains

  module procedure assert

    if (enforce_assertions) then



      if (.not. assertion) error stop description

    end if

  end procedure

end submodule assert_s
module sp_resource_m
  implicit none

  private
  public :: sp_resource_t

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
  implicit none

  private
  public :: sp_reference_counter_t

  type sp_reference_counter_t
    private
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

    module function construct(object) result(sp_reference_counter)
      implicit none
      class(sp_resource_t), intent(in) :: object
      type(sp_reference_counter_t) sp_reference_counter
     end function

  end interface

  interface

    pure module function reference_count(self) result(counter)
      implicit none
      class(sp_reference_counter_t), intent(in) :: self
      integer counter
    end function

    module subroutine grab(self)
      implicit none
      class(sp_reference_counter_t), intent(inout) :: self
    end subroutine

    module subroutine release(self)
      implicit none
      class (sp_reference_counter_t), intent(inout) :: self
    end subroutine

    module subroutine assign_sp_reference_counter(lhs, rhs)
      implicit none
      class(sp_reference_counter_t), intent(inout) :: lhs
      class(sp_reference_counter_t), intent(in) :: rhs
    end subroutine

  end interface

contains

  subroutine finalize(self)
    type(sp_reference_counter_t), intent(inout) :: self
    if (associated(self%count_)) call self%release
  end subroutine

end module sp_reference_counter_m
submodule(sp_reference_counter_m) sp_reference_counter_s
  use assert_m, only : assert
  implicit none

contains

  module procedure reference_count
    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")
    counter = self%count_
  end procedure

  module procedure construct
    print *,"sp_reference_counter_s(construct): allocate(sp_reference_counter%count_, source=0)"
    allocate(sp_reference_counter%count_, source=0)
    allocate(sp_reference_counter%object_, source=object)
    call sp_reference_counter%grab
  end procedure

  module procedure grab
    call assert(associated(self%count_),"sp_reference_counter_t%grab: associated(self%count_)")
    print *,"sp_reference_counter_s(grab): self%count_ = self%count_ + 1"
    self%count_ = self%count_ + 1
    print *,"sp_reference_counter_s(grab): self%count_ = ", self%count_
  end procedure

  module procedure release

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
  end procedure

  module procedure assign_sp_reference_counter
    call assert(associated(rhs%count_),"sp_reference_counter_s(assign_sp_reference_counter): associated(self%count_)")
    print *,"sp_reference_counter_s(assign_sp_reference_counter): lhs%count_ => rhs%count_"
    lhs%count_ => rhs%count_
    lhs%object_ => rhs%object_
    call lhs%grab
  end procedure

end submodule sp_reference_counter_s
module sp_smart_pointer_m
  use sp_resource_m, only: sp_resource_t
  use sp_reference_counter_m, only: sp_reference_counter_t
  implicit none

  private
  public :: sp_smart_pointer_t

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

end module sp_smart_pointer_m
module shallow_m
    use sp_smart_pointer_m, only: sp_smart_pointer_t

    implicit none
    private
    public :: shallow_t, resource_freed

    type, extends(sp_smart_pointer_t) :: shallow_t
        integer, pointer :: ref => null()
    contains
        procedure :: free
    end type

    interface shallow_t
        module procedure construct
    end interface

    integer, allocatable, target, save :: resource
    logical, save :: resource_freed = .false.

contains
    function construct() result(shallow)
        type(shallow_t) :: shallow

        resource = 42
        shallow%ref => resource
        call shallow%start_counter
    end function

    impure elemental subroutine free(self)
        class(shallow_t), intent(inout) :: self

        deallocate(resource)
        nullify(self%ref)
        resource_freed = .true.
    end subroutine

end module
submodule(sp_smart_pointer_m) sp_smart_pointer_s



  implicit none

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

end submodule

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

  function check_creation() result(test_passes)
    logical test_passes
    type(object_t) :: object

    object = object_t()

    if (allocated(the_resource)) then
        test_passes = the_answer == the_resource
    else
        test_passes = .false.
    end if
  end function

end module sp_smart_pointer_test_m

  use sp_smart_pointer_test_m
  implicit none
  associate(check => check_creation())
    print *,check
  end associate
end
