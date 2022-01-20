module test_type_mod

  type :: my_test_type
    integer, allocatable :: i
  contains
    final :: delete_test_type
  end type my_test_type

  interface my_test_type
    module procedure  new_test_type_object
  end interface my_test_type

contains

  subroutine delete_test_type(this)
    type(my_test_type) :: this

    write(*,*) 'Called delete_test_type'
    if (allocated(this%i)) deallocate(this%i)

  end subroutine delete_test_type
    
  
  function new_test_type_object(item) result(res)
    type(my_test_type)  :: res
    integer, intent(in) :: item
    !Allocation on assignment
    res%i=item
  end function new_test_type_object


end module test_type_mod

module target_mod
  use test_type_mod
  type :: outer_type
    type(my_test_type), allocatable  :: test_item
  end type outer_type
  
contains

  subroutine new_outer_type(outer,item)
    type(outer_type), intent(out) :: outer
    integer :: item
    
    allocate(outer%test_item)
    write(*,*) 'Assigning outer%test_item'
    outer%test_item = my_test_type(itemi)
    write(*,*) 'End of new_outer_type'
  end subroutine new_outer_type

end module target_mod

program testfinal
  use target_mod

  implicit none

  integer :: i=10
  type(outer_type), allocatable  :: wrapper

  write(*,*) 'Allocating wrapper '
  allocate(wrapper)
  write(*,*) 'Calling new_outer_type '  
  call new_outer_type(wrapper,i)
  write(*,*) 'DeAllocating wrapper '
  deallocate(wrapper)
  
end program testfinal
