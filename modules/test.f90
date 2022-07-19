program test
  use integral
  implicit none

  real*8 area, xmin, xmax
  integer n

  xmin = 2.0
  xmax = 4.0
  n = 100000000

  area = calculate_integral_trapezoid(f, xmin, xmax, n)
  print *, area

contains
  function f(x) result(y)
    implicit none

    real*8 :: x, y

    y = x**3

  end function f
end program test
