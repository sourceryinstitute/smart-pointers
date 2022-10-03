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
      ,test_result_t("removes the resource when the object goes out of scope", check_deletion()) &
      ,test_result_t("copy points to the same resource as the original", check_copy()) &
      ,test_result_t("has zero references after a shallow copy goes out of scope", check_shallow_copy()) &
    ]   

  end function

  function construct() result(object)
    type(object_t) :: object

    if (.not. allocated(the_resource)) allocate(the_resource, source=the_answer)
    object%ref => the_resource
    object%ref = the_answer
    call object%start_counter
  end function

  subroutine free(self)
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
#ifdef XLF
    type(object_t) :: original

    original = object_t()
#else
    associate(original => object_t())
#endif
      reference = original

      block 
        type(object_t) :: declared, reference_to_declared

        if (scan(compiler_version(),"GCC ")==1) then
          test_passes = associated(original%ref, reference%ref) .and. .false. ! intentional failure due to the message below
          print *, "  (skipped copy of declared reference due to a gfortran bug that would cause a segmentation fault)"
        else
#ifndef __GFORTRAN__
          declared = object_t() ! compiling with gfortran generates a runtime error even when this line doesn't execute
#endif
          reference_to_declared = declared
          test_passes = associated(original%ref, reference%ref) .and. associated(declared%ref, reference_to_declared%ref)
        end if
      end block
#ifndef XLF
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
