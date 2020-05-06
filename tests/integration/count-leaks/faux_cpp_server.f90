module faux_cpp_server
  use iso_c_binding ,only: c_int,c_double
  implicit none
  private
  public :: cpp_new_vector
  public :: cpp_delete_vector
  public :: cpp_add_vectors
  public :: cpp_subtract_vectors
  public :: cpp_rescale_vector
  public :: num_leaks
  integer(c_int), save :: unique_id=0_c_int
  integer, parameter :: max_objects=1000
  real(c_double), parameter :: deallocated=tiny(0._c_double)
  real(c_double), parameter :: never_allocated=huge(0._c_double)
  real(c_double), dimension(3,max_objects) :: cpp_vector=never_allocated
  interface cpp_new_vector ! public constructor
    module procedure initialize_vector,copy_vector
  end interface
contains
  function uninitialized_vector_id() result(id) 
    integer(c_int) :: id
    unique_id = unique_id + 1_c_int
    id = unique_id
  end function
  function initialize_vector(x,y,z) result(id) bind(C)
    integer(c_int) :: id
    real(c_double), intent(in), value :: x,y,z
    id = uninitialized_vector_id()
    if (id>max_objects) stop 'Out of memory.'
    cpp_vector(1,id)= x
    cpp_vector(2,id)= y
    cpp_vector(3,id)= z
  end function
  function copy_vector(original_id) result(id) bind(C)
    integer(c_int), intent(in) :: original_id
    integer(c_int) :: id
    id = uninitialized_vector_id()
    if (id>max_objects) stop 'Out of memory.'
    cpp_vector(:,id) =  cpp_vector(:,original_id)
  end function
  subroutine cpp_delete_vector(id) bind(C)
    integer(c_int),value :: id
    cpp_vector(:,id) = tiny(0._c_double)
  end subroutine
  function cpp_add_vectors(lhs_id,rhs_id) result(total_id) bind(C)
    integer(c_int),value :: lhs_id,rhs_id
    integer(c_int)       :: total_id
    total_id = uninitialized_vector_id()
    cpp_vector(:,total_id)=cpp_vector(:,lhs_id)+cpp_vector(:,rhs_id)
  end function
  function cpp_subtract_vectors(lhs_id,rhs_id) result(difference_id) bind(C)
    integer(c_int),value :: lhs_id,rhs_id
    integer(c_int)       :: difference_id
    difference_id = uninitialized_vector_id()
    cpp_vector(:,difference_id)=cpp_vector(:,lhs_id)-cpp_vector(:,rhs_id)
  end function
  function cpp_rescale_vector(lhs_id,rhs) result(product_id) bind(C)
    integer(c_int),value :: lhs_id
    integer(c_int) :: product_id
    real(c_double),value :: rhs
    product_id = uninitialized_vector_id()
    cpp_vector(:,product_id)=cpp_vector(:,lhs_id)*rhs
  end function 
  integer(c_int) function num_leaks()
    integer(c_int) :: num_never_allocated,num_deallocated
    num_never_allocated = count( cpp_vector == huge(0._c_double) )/size(cpp_vector,1)
    num_deallocated = count( cpp_vector == tiny(0._c_double) )/size(cpp_vector,1)
    num_leaks = max_objects - (num_never_allocated + num_deallocated)
  end function
end module
