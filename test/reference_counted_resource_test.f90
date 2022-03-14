module reference_counted_resource_test
  use reference_counted_resource_m, only : reference_counted_resource_t
  use vegetables, only: result_t, test_item_t, assert_that, describe, it, assert_equals

  implicit none
  private
  public :: test_reference_counted_resource

  type, extends(reference_counted_resource_t) :: resource_t
  contains
    procedure :: free_resource
  end type

  interface resource_t
    module function construct() result(resource)
      implicit none
      type(resource_t) resource
    end function
  end interface

  enum, bind(C)
    enumerator :: never_referenced, not_freed, freed
  end enum
  integer, parameter :: max_resources=1000
  integer :: ref_status(max_resources) = never_referenced
  
contains

  module function construct() result(resource)
    type(resource_t) resource
    call resource%start_counter
  end function

  subroutine free_resource(self)
    class(resource_t), intent(inout) :: self
  end subroutine

  function test_reference_counted_resource() result(tests)
    type(test_item_t) :: tests

    tests = &
      describe( &
        "A reference_counted_resource", &
        [ it("does not leak constructed, assigned, and then explicitly freed", check_for_leaks) &
      ])
  end function

  function check_for_leaks() result(result_)
    type(result_t) :: result_
    type(resource_t) resource

    resource = resource_t()
    call resource%free_resource

    associate(num_never_referenced => count( ref_status == never_referenced ), num_freed => count( ref_status == freed ))
     associate(num_leaks => max_resources - (num_never_referenced + num_freed))
        result_ = assert_equals(0, num_leaks)
      end associate
    end associate
  end function

end module reference_counted_resource_test
