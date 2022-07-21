program main
  implicit none

  external calculate_derivative, f
  real*8 :: calculate_derivative, f, derivative, x
  integer :: n

  ! while this does indeed calculate the derivative, it is not precise and
  ! doesn't get more accurate the more values are added to the vector

end program main

function f(x) result(y)
  implicit none

  real*8 :: x, y
  y = x**2
end function f

function calculate_derivative(f, x, n) result(derivative)
  implicit none

  interface
     function f(x) result(y)
       real*8 :: x, y
     end function f
  end interface

  real*8, intent(in) :: x
  real*8 :: derivative
  real*8 :: function_values(n)

  real*8 :: step, sum

  integer, intent(in) :: n
  integer :: i

  step = 1.0/2**20
  sum = 0.0

  function_values = [(f(x + (step * dfloat(i))), i = 1, n)]

  do i = 1, n-1
     sum = sum + (function_values(i+1) - function_values(i))
  end do

  derivative = sum / (step*n)
end function calculate_derivative
