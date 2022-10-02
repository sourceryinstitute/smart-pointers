program main
  use compiler_test_m, only : compiler_test_t 
  use sp_smart_pointer_test_m, only : sp_smart_pointer_test_t
  implicit none

  type(compiler_test_t) compiler_test
  type(sp_smart_pointer_test_t) sp_smart_pointer_test

  call compiler_test%report()
  call sp_smart_pointer_test%report()
end program
