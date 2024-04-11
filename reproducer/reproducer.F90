module test_result_m
  !! Define a basic abstraction for describe test intentions and results
  implicit none

  private
  public :: test_result_t

  type test_result_t
    private
    character(len=:), allocatable :: description_
    logical outcome_
  contains
    procedure :: characterize
  end type

  interface test_result_t

    pure module function construct(description, outcome) result(test_result)
      implicit none
      character(len=*), intent(in) :: description
      logical, intent(in) :: outcome
      type(test_result_t) test_result 
    end function

  end interface

  interface

    pure module function characterize(self) result(characterization)
      implicit none
      class(test_result_t), intent(in) :: self
      character(len=:), allocatable :: characterization
    end function

  end interface

end module test_result_m

submodule(test_result_m) test_result_s
  implicit none

contains

    module procedure construct
      test_result%description_ = description
      test_result%outcome_ = outcome
      !print *,"test_result_s(construct): ending"
    end procedure

    module procedure characterize
      characterization = merge("Pass: ", "Fail: ", self%outcome_) // self%description_
    end procedure

end submodule test_result_s
module test_m
  !! Define an abstract test type and test-result template method
  use test_result_m, only : test_result_t
  implicit none

  private
  public :: test_t

  type, abstract :: test_t
  contains
    procedure(subject_interface), nopass, deferred :: subject
    procedure(results_interface), nopass, deferred :: results
    procedure :: report
  end type

  abstract interface

    pure function subject_interface() result(specimen)
      character(len=:), allocatable :: specimen
    end function

    function results_interface() result(test_results)
      import test_result_t
      type(test_result_t), allocatable :: test_results(:)
    end function

  end interface

  interface

    module subroutine report(test)
      implicit none
      class(test_t), intent(in) :: test
    end subroutine

  end interface

end module test_m

submodule(test_m) test_s
#ifdef XLF
  use test_result_m, only : test_result_t
#endif
  implicit none

contains

  module procedure report
    integer i
    type(test_result_t), allocatable :: test_results(:)

    print *
    print *, test%subject()

    test_results = test%results()
    do i=1,size(test_results)
      print *,"  ",test_results(i)%characterize()
    end do
  end procedure

end submodule test_s
module assert_m
  !! Enforce logical assertions that can be toggled on/off at compile-time
  !! To turn off assertions, building with the flag -DUSE_ASSERTIONS=.false.
  implicit none

  private
  public :: assert

#ifndef USE_ASSERTIONS
# define USE_ASSERTIONS .true.
#endif
  logical, parameter :: enforce_assertions = USE_ASSERTIONS

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
#ifdef XLF
      if (.not. assertion) error stop 999
#else
      if (.not. assertion) error stop description
#endif
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
#ifdef XLF
  use sp_reference_counter_m, only : sp_reference_counter_t 
#endif
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
  use iso_fortran_env, only : compiler_version
  use sp_smart_pointer_m, only: sp_smart_pointer_t
  use shallow_m, only : shallow_t, resource_freed
  use test_result_m, only : test_result_t
  use test_m, only : test_t

  implicit none
  private
  public :: sp_smart_pointer_test_t

  type, extends(test_t) :: sp_smart_pointer_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

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

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A smart_pointer"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ & 
       test_result_t("creates a resource when constructed", check_creation()) &
      !,test_result_t("removes the resource when the object goes out of scope", check_deletion()) &
      !,test_result_t("copy points to the same resource as the original", check_copy()) &
      !,test_result_t("has zero references after a shallow copy goes out of scope", check_shallow_copy()) &
    ]   

  end function

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

    print *,"object = object_t()"
    object = object_t()

    if (allocated(the_resource)) then
        test_passes = the_answer == the_resource
    else
        test_passes = .false.
    end if
    print *,"check_creation() ending"
  end function

  function check_deletion() result(test_passes)
    logical test_passes

    block
        type(object_t) :: object

        object = object_t()
    end block

    test_passes = .not. allocated(the_resource)
  end function

  function check_copy() result(test_passes)
    logical test_passes
    type(object_t) :: reference
#if defined(XLF) || defined(_CRAYFTN)
    type(object_t) :: original

    original = object_t()
#else
    associate(original => object_t())
#endif
      reference = original

      block 
        type(object_t) :: declared, reference_to_declared

        declared = object_t() ! compiling with gfortran generates a runtime error even when this line doesn't execute
        reference_to_declared = declared
        test_passes = associated(original%ref, reference%ref) .and. associated(declared%ref, reference_to_declared%ref)
      end block
#if !defined(XLF) && !defined(_CRAYFTN)
    end associate
#endif

  end function
  
  function check_shallow_copy() result(test_passes)
     logical test_passes
   
    block 
      type(shallow_t) shallow_copy

      associate(original => shallow_t())
        shallow_copy = original
      end associate
    end block

    test_passes = resource_freed

  end function

end module sp_smart_pointer_test_m
program main
  use sp_smart_pointer_test_m, only : sp_smart_pointer_test_t
  implicit none
  type(sp_smart_pointer_test_t) sp_smart_pointer_test
  call sp_smart_pointer_test%report()
end program
