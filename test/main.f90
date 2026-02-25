program main
  use compiler_test_m, only : compiler_test_t
  use sp_smart_pointer_test_m, only : sp_smart_pointer_test_t
  implicit none

  type(compiler_test_t) compiler_test
  type(sp_smart_pointer_test_t) sp_smart_pointer_test
  integer :: failures

  failures = compiler_test%report()
  failures = failures + sp_smart_pointer_test%report()
  if (failures > 0) error stop
end program
