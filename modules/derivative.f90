module derivative
  implicit none

contains

  function calculate_derivative(f, x) result(der)
    implicit none

    real*8 :: x, der
    real*8 :: h
    integer :: n, i

    interface
       function f(x) result(y)
         real*8 :: x, y
       end function f
    end interface

    n = 20
    h = 1.0

    do i = 1, n
       der = (f(x + h) - f(x - h))/(2.0*h)
       h = h / 2.0
    end do

  end function calculate_derivative

end module derivative
