module usage_test
    use reference_counter_m, only: ref_reference_t
    use veggies, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it
    use shallow_m, only : shallow_t, resource_freed

    implicit none
    private
    public :: test_usage

    type, extends(ref_reference_t) :: object_t
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

        allocate(the_resource)
        object%ref => the_resource
        object%ref = the_answer
        call object%start_ref_counter
    end function

    subroutine free(self)
        class(object_t), intent(inout) :: self

        deallocate(the_resource)
        nullify(self%ref)
    end subroutine

    function test_usage() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "A reference-counted object", &
                [ it("creates a resource when constructed", check_creation) &
                , it("removes the resource when the object goes out of scope", check_deletion) &
                , it("copy points to the same resource as the original", check_copy) &
                , it("has zero references afrer a shallow copy goes out of scope", check_shallow_copy) &
                ])
    end function

    function check_creation() result(result_)
        type(result_t) :: result_

        type(object_t) :: object

        object = object_t()
        if (allocated(the_resource)) then
            result_ = assert_equals(the_answer, the_resource)
        else
            result_ = fail("resource not retained")
        end if
    end function

    function check_deletion() result(result_)
        type(result_t) :: result_

        block
            type(object_t) :: object

            object = object_t()
        end block
        result_ = assert_not(allocated(the_resource))
    end function

    function check_copy() result(result_)
        type(result_t) :: result_

        type(object_t) :: object1, object2

        object1 = object_t()
        object2 = object1
        result_ = assert_that(associated(object2%ref, object1%ref))
    end function
    
    function check_shallow_copy() result(result_)
      type(result_t) :: result_
      
      block 
        type(shallow_t) shallow_copy

        associate(original => shallow_t())
          shallow_copy = original
        end associate
      end block

      result_ = assert_that(resource_freed)
    end function
end module
