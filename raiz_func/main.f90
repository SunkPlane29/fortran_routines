program root
  use derivative
  implicit none

  real*8 :: x0

  x0 = -2.0

  print *, calculate_root(x0)

contains

  function calculate_root(x0) result (xn)
    implicit none

    real*8 :: x0, xn, precision

    xn = x0
    precision = 1.0e-6

    do while (abs(f1(x0)) > precision)
       xn = xn - f1(xn)/calculate_derivative(f1, xn)
    end do
  end function calculate_root

  function f1(x) result(y)
    implicit none

    real*8 :: x, y

    y = x**2 - x - 1
  end function f1

end program root
