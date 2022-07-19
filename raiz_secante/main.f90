program main
  implicit none

  real*8 next_x, f1
  real*8 x0, x1,next, precision

  precision = 1.0e-6
  x0 = -3.0
  x1 = 3.0

  do while (abs(f1(next)) > precision)
     next = next_x(x0, x1)
     x0 = x1
     x1 = next
  end do

  print *, next

end program main

function next_x(x0, x1) result(next)
  implicit none

  real*8 f1
  real*8 x0, x1, next

  next = f1(x1)/((f1(x1) - f1(x0))/(x1 - x0))

end function next_x

function f1(x) result(y)
  implicit none

  real*8 x, y
  y = x**2 - x - 1

end function f1
