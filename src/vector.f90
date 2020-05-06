module vector_implementation
  use iso_c_binding, only : c_int,c_double
  use universal_interface, only: universal
  use faux_cpp_server
  implicit none

  private                ! Hide everything by default
  public :: vector       ! Expose type, constructor, type-bound procedures

  ! Shadow object
  type, extends(universal) :: vector 
    private
    integer(c_int) :: id ! C++ object identification tag
  contains
    procedure :: sum
    procedure :: difference
    procedure :: product
    procedure :: ratio
    procedure :: integral
    generic :: operator(+) => sum
    generic :: operator(-) => difference
    generic :: operator(*) => product
    generic :: operator(/) => ratio
    generic :: operator(.integral.) => integral
    procedure :: cpp_delete => call_cpp_delete_vector
  end type

  ! Constructors
  interface vector   
    module procedure new_vector,default_vector,duplicate
  end interface

contains

  type(vector) function default_vector(id)
    integer(c_int),intent(in) :: id
    default_vector%id = id
    call default_vector%register_self
  end function

  type(vector) function new_vector(vec)
    real(c_double), dimension(3) :: vec
    integer(c_int) :: new_vector_id
    new_vector = vector(cpp_new_vector(vec(1),vec(2),vec(3)))
  end function

  type(vector) function duplicate(original)
    type(vector), intent(in) :: original
    duplicate = vector(cpp_new_vector(original%id))
  end function

  type(vector) function sum(lhs,rhs) 
    class(vector), intent(in)  :: lhs,rhs
    sum = vector(cpp_add_vectors(lhs%id,rhs%id))
  end function

  type(vector) function difference(lhs,rhs) 
    class(vector) ,intent(in)  :: lhs,rhs
    difference = vector(cpp_subtract_vectors(lhs%id,rhs%id))  
  end function

  type(vector) function product(lhs,rhs) 
    class(vector) ,intent(in)  :: lhs
    real(c_double), intent(in)  :: rhs
    product = vector(cpp_rescale_vector(lhs%id,rhs))
  end function

  type(vector) function ratio(lhs,rhs) 
    class(vector), intent(in)  :: lhs
    real(c_double), intent(in) :: rhs
    ratio = vector(cpp_rescale_vector(lhs%id,1._c_double/rhs))  
  end function

  type(vector) function integral(rhs) ! Explicit Euler quadrature
    class(vector) ,intent(in)  :: rhs
    integral = vector(rhs) 
  end function

  subroutine call_cpp_delete_vector(this)
    class(vector),intent(inout) :: this
    call cpp_delete_vector(this%id)
  end subroutine

end module
