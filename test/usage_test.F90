module usage_test
    use iso_fortran_env, only : compiler_version
    use sp_smart_pointer_m, only: sp_smart_pointer_t
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
        !object%ref = the_answer
        call object%start_ref_counter
    end function

    subroutine free(self)
        class(object_t), intent(inout) :: self

        if (allocated(the_resource)) deallocate(the_resource)
        nullify(self%ref)
    end subroutine

    function test_usage() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "A reference-counted object", &
                [ it("creates a resource when constructed", check_creation) &
                , it("removes the resource when the object goes out of scope", check_deletion) &
                , it("copy points to the same resource as the original", check_copy) &
                , it("has zero references after a shallow copy goes out of scope", check_shallow_copy) &
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
        type(object_t) :: reference

        associate(original => object_t())
          reference = original

          block 
            type(object_t) declared, reference_to_declared

            if (scan(compiler_version(),"GCC ")==1) then
              result_ = assert_that(associated(original%ref, reference%ref)) .and. &
                        fail("skipped copy of declared reference due to a gfortran bug that would cause a segmentation fault")
            else
#ifndef __GFORTRAN__
              declared = object_t() ! compiling with gfortran generates a runtime error even when this line doesn't execute
#endif
              reference_to_declared = declared
              result_ = assert_that(associated(original%ref, reference%ref)) .and.  &
                        assert_that(associated(declared%ref, reference_to_declared%ref))
            end if
          end block
        end associate

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
