program main
  use compiler_test_m, only : compiler_test_t 
  use sp_smart_pointer_test_m, only : sp_smart_pointer_test_t
  implicit none

  type(compiler_test_t) compiler_test
  type(sp_smart_pointer_test_t) sp_smart_pointer_test
  integer i

  print *
  print *, compiler_test%subject()

  associate(compiler_test_results => compiler_test%results())
    do i=1,size(compiler_test_results)
      print *,"  ",compiler_test_results(i)%characterize()
    end do
  end associate

  print *
  print *, sp_smart_pointer_test%subject()

  associate(sp_smart_pointer_test_results => sp_smart_pointer_test%results())
    do i=1,size(sp_smart_pointer_test_results)
      print *,"  ",sp_smart_pointer_test_results(i)%characterize()
    end do
  end associate
end program
