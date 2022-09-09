! Generated by cart. DO NOT EDIT
program main
    implicit none

    if (.not.run()) stop 1
contains
    function run() result(passed)
        use compiler_test, only: &
                compiler_ref_reference => &
                    test_ref_reference
        use usage_test, only: &
                usage_usage => &
                    test_usage
        use veggies, only: test_item_t, test_that, run_tests



        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = compiler_ref_reference()
        individual_tests(2) = usage_usage()
        tests = test_that(individual_tests)


        passed = run_tests(tests)

    end function
end program